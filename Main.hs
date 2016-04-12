{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Data.Aeson (FromJSON, ToJSON, (.=), (.:), (.:?))
import Data.Char
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Scientific
import Data.String.Conv
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Codec.Compression.GZip
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import GHC.Generics
import STMContainers.Map (Map)
import System.Environment
import System.Exit
import System.IO
import Text.Read
import Web.Scotty (ScottyM)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified ListT
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import qualified Pipes.ByteString as P
import qualified Pipes.GZip as P
import qualified Pipes.HTTP as P
import qualified STMContainers.Map as Map
import qualified System.Systemd.Daemon as Systemd
import qualified Web.Scotty as Scotty

------------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------------

data FieldMatch = FieldMatch
  { match :: Text
  , field :: Text
  , value :: Text
  } deriving (Generic, FromJSON)

data Match = MatchField FieldMatch | MatchAny [Match] | MatchAll [Match]
  deriving Generic

instance FromJSON Match where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
    { Aeson.constructorTagModifier = \(c:cs) -> toLower c : cs
    , Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }

data Metric = Metric
  { name :: Name
  , matches :: Match
  , setTags :: Maybe (HashMap Name Text)
  , mapTags :: Maybe (HashMap Name Text)
  , inheritTags :: Maybe [Name]
  , incrementBy :: Maybe (Text, Double)
  , count :: Maybe (Text, Double) -- TODO: rename to 'const'?
  } deriving Generic

instance FromJSON Metric where
  parseJSON = Aeson.withObject "metric" $ \o ->
    let
      scaledField :: Maybe Aeson.Object -> Aeson.Parser (Maybe (Text, Double))
      scaledField Nothing = pure Nothing
      scaledField (Just o') = do
        f <- o' .: "field"
        m <- o' .:? "multiplier"
        return (Just (f, fromMaybe 1.0 m))
      matchFld = MatchField <$> o .: "matchField"
      matchAny = MatchAny <$> o .: "matchAny"
      matchAll = MatchAll <$> o .: "matchAll"
      matches = matchFld <|> matchAny <|> matchAll
    in
    Metric <$>
      o .: "name" <*>
      matches <*>
      o .:? "setTags" <*>
      o .:? "mapTags" <*>
      o .:? "inheritTags" <*>
      ((o .:? "incrementBy") >>= scaledField) <*>
      ((o .:? "count") >>= scaledField)

data Config = Config
  { port :: Int
  , logHost :: String
  , logPort :: Int
  , metricsHost :: String
  , metricsPort :: Int
  , metricsInterval :: Int -- milliseconds
  , metricsMaxBodySize :: Maybe Int64 -- bytes
  , metricsMaxChunkSize :: Maybe Int64 -- bytes
  , metricsMaxAge :: Maybe Int64 -- seconds
  , metrics :: [Metric]
  } deriving (Generic, FromJSON)

validateMetric :: Metric -> IO ()
validateMetric Metric {setTags, mapTags, inheritTags} =
  when (nbTags == 0) (die "A metric must have at least one tag")
  where
    nbTags = sum
      [ maybe 0 HashMap.size setTags
      , maybe 0 HashMap.size mapTags
      , maybe 0 length inheritTags
      ]

validateConfig :: Config -> IO ()
validateConfig c = mapM_ validateMetric (metrics c)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- B.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Left err -> error err
    Right config -> do
      validateConfig config
      pure config

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

getTimestamp :: IO Int64
getTimestamp = round <$> getPOSIXTime

------------------------------------------------------------------------------
-- Log and metric types
------------------------------------------------------------------------------

-- | OpenTSDB metric/tag name
newtype Name = Name Text deriving (Eq, Generic, Hashable)

validChar :: Char -> Aeson.Parser ()
validChar c
  | isAlpha c || isDigit c || c `elem` ['-', '_', '.', '/'] = pure ()
  | otherwise = fail ("Forbidden character in tag/metric name: " ++ [c])

validName :: Text -> Aeson.Parser Name
validName txt = mapM_ validChar (Text.unpack txt) >> pure (Name txt)

instance FromJSON Name where
  parseJSON = Aeson.withText "metric name" validName

instance ToJSON Name where
  toJSON (Name s) = Aeson.toJSON s

-- | Transform the keys and values of a 'H.HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2) -> HashMap k1 v1 -> HashMap k2 v2
mapKeyVal fk kv = HashMap.foldrWithKey (\k v -> HashMap.insert (fk k) (kv v)) HashMap.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.HashMap'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}

instance FromJSON (HashMap Name Text) where
  parseJSON v = mapKey Name <$> Aeson.parseJSON v

instance ToJSON (HashMap Name Text) where
  toJSON m = Aeson.object [(k, Aeson.String v) | (Name k, v) <- HashMap.toList m]

type LogEvent = HashMap Text Aeson.Value

-- | OpenTSDB DataPoint
data DataPoint = DataPoint
  { dpMetric :: Name
  , dpTimestamp :: Int64
  , dpValue :: Int64
  , dpTags :: HashMap Name Text
  } deriving Generic

instance ToJSON DataPoint where
  toJSON DataPoint {dpMetric, dpTimestamp, dpValue, dpTags} =
    Aeson.object
      [ "metric" .= dpMetric
      , "timestamp" .= dpTimestamp
      , "value" .= dpValue
      , "tags" .= dpTags
      ]

------------------------------------------------------------------------------
-- Counters
------------------------------------------------------------------------------

data CounterKey = CounterKey
  { metricName :: Name
  , tags :: [(Name, Text)]
  } deriving (Generic, Hashable, Eq)

data CounterValue = CounterValue
  { counterValue :: Int64
  , lastChanged :: Int64
  }

type Counters = Map CounterKey CounterValue

-- Creates a new counter on-the-fly if none is found
-- Deletes counter if it is stale (with regards to metricsMaxAge)
bumpCounter :: Maybe Int64 -> Int64 -> Counters -> (CounterKey, Int64 -> Int64) -> IO ()
bumpCounter metricsMaxAge timestamp counters (key, f) = atomically $ do
  mCounter <- Map.lookup key counters
  case mCounter of
    Nothing -> Map.insert (CounterValue {counterValue = f 0, lastChanged = timestamp}) key counters
    Just (CounterValue {counterValue,lastChanged}) -> do
      let counterValue' = f counterValue
          lastChanged' = if counterValue' == counterValue
                         then lastChanged
                         else timestamp
          value = CounterValue
                    { counterValue = counterValue'
                    , lastChanged = lastChanged'
                    }
      if maybe False (timestamp - lastChanged' >) metricsMaxAge
      then Map.delete key counters
      else Map.insert value key counters

countersToList :: Counters -> IO [(CounterKey, CounterValue)]
countersToList counters = atomically (ListT.toList (Map.stream counters))

------------------------------------------------------------------------------
-- Metrics thread
------------------------------------------------------------------------------

counterToDataPoint :: Int64 -> (CounterKey, CounterValue) -> DataPoint
counterToDataPoint timestamp (CounterKey {metricName, tags}, CounterValue {counterValue}) =
  DataPoint
    { dpMetric = metricName
    , dpTimestamp = timestamp
    , dpValue = counterValue
    , dpTags = HashMap.fromList tags
    }

chunkRecords :: Int64 -> [BL.ByteString] -> [BL.ByteString]
chunkRecords n = unfoldr (\case [] -> Nothing; recs -> Just (splitRecords n recs))

splitRecords :: Int64 -> [BL.ByteString] -> (BL.ByteString, [BL.ByteString])
splitRecords maxSize records =
  case records of
    [] -> error "Cannot happen"
    rec : _ | BL.length rec + 2 > maxSize ->
      error "Fatal error: metricsMaxBodySize too small for a single datapoint"
    _ ->
      let (recs, rest) = go 2 mempty False records in
      let builder = Builder.char8 '[' <> recs <> Builder.char8 ']' in
      let chunk = Builder.toLazyByteString builder in
      (chunk, rest)
  where
    go _ _ _ [] = error "Cannot happen"
    go bytes chunk comma (rec : recs)
      | bytes + BL.length rec > maxSize = (chunk, rec : recs)
      | [] <- recs = (chunk <> comma_ <> Builder.lazyByteString rec, [])
      | otherwise = go (bytes + BL.length rec + 1) (chunk <> comma_ <> Builder.lazyByteString rec) True recs
      where comma_ | comma = Builder.char8 ','
                   | otherwise = mempty

chunkedPayload :: Int64 -> BL.ByteString -> Wreq.Payload
chunkedPayload chunkSize =
    Wreq.Raw "application/json"
  . P.stream
  . P.chunksOf' chunkSize
  . P.compress P.defaultCompression
  . P.fromLazy

sendMetrics :: Config -> Counters -> IO ()
sendMetrics config counters = do
  cs <- countersToList counters
  timestamp <- getTimestamp
  let points = map (counterToDataPoint timestamp) cs
  unless (null points) $ do
    let payloads =
          case metricsMaxBodySize config of
            Nothing -> [Aeson.encode points]
            Just n -> chunkRecords n (map Aeson.encode points)
    mapM_ (sendPayload config) payloads

sendPayload :: Config -> BL.ByteString -> IO ()
sendPayload Config {metricsHost, metricsPort, metricsMaxChunkSize} body =
  let url = "http://" ++ metricsHost ++ ":" ++ show metricsPort ++ "/api/put" in
  let opts = Wreq.defaults & Wreq.header "Content-Encoding" .~ ["gzip"] in
  void $ case metricsMaxChunkSize of
    Nothing -> Wreq.postWith opts url (compress body)
    Just n -> Wreq.postWith opts url (chunkedPayload n body)

metricsThread :: Config -> Counters -> IO ()
metricsThread config counters = forever $ do
  threadDelay (metricsInterval config * 1000)
  forkIO (sendMetrics config counters)

------------------------------------------------------------------------------
-- Matching
------------------------------------------------------------------------------

-- Logging and IO, using StateT instead of the broken WriterT
type LogIO a = StateT [Text] IO a

say :: Text -> LogIO ()
say !txt = modify' (txt :)

matchError :: Show a => Text -> Text -> a -> LogIO (Maybe b)
matchError msg key val = do
  say (msg <> " Not matching. " <> toS (show key) <> ": " <> toS (show val))
  pure Nothing

matchTag :: Text -> LogEvent -> LogIO (Maybe Text)
matchTag key fields =
  case HashMap.lookup key fields of
    Nothing -> pure Nothing
    Just (Aeson.String str) -> pure (Just str)
    Just val -> matchError "Non-string tags are not supported." key val

matchField :: LogEvent -> FieldMatch -> LogIO Bool
matchField event FieldMatch {match, field, value} =
  case HashMap.lookup field event of
    Nothing -> pure False
    Just (Aeson.Number _) -> pure True
    Just (Aeson.String str) -> pure $
      case match of
        "exact" -> value == str
        "contains" -> value `Text.isInfixOf` str
        "startsWith" -> value `Text.isPrefixOf` str
        "!exact" -> value /= str
        "!contains" -> not (value `Text.isInfixOf` str)
        _ -> error "unsupported match type"
    Just val -> const False <$> matchError "Only string or number fields supported." field val

-- Matches numbers or strings that can be converted to numbers
matchNumberishField :: Text -> LogEvent -> LogIO (Maybe Double)
matchNumberishField key event =
  case HashMap.lookup key event of
    Nothing -> pure Nothing
    Just (Aeson.Number x) -> pure (Just (toRealFloat x))
    Just (Aeson.String str) ->
      case readMaybe (toS str) of
        Nothing -> matchError "Couldn't parse string field as double." key str
        x -> pure x
    Just val -> matchError "Only string or number fields supported." key val

matchCountingDef :: LogEvent -> Maybe (Text, Double) -> Maybe (Text, Double) -> LogIO (Maybe (Int64 -> Int64))
matchCountingDef event incrementBy count =
  case (incrementBy, count) of
    (Nothing, Nothing) -> pure (Just (+1))
    (Just (field, mul), _) -> apply (+) mul field
    (Nothing, Just (field, mul)) -> apply const mul field
  where
    apply :: (Int64 -> Int64 -> Int64) -> Double -> Text -> LogIO (Maybe (Int64 -> Int64))
    apply f mul field = do
      mN <- matchNumberishField field event
      case mN of
        Nothing -> pure Nothing
        Just n -> pure (Just (f (round (mul*n))))

matchRec :: LogEvent -> Match -> LogIO Bool
matchRec event (MatchField m) = matchField event m
matchRec event (MatchAny ms) = anyM (matchRec event) ms
matchRec event (MatchAll ms) = allM (matchRec event) ms

matchMetric :: LogEvent -> Metric -> LogIO (Maybe (Int64 -> Int64))
matchMetric event Metric {matches, incrementBy, count} = do
  isMatch <- matchRec event matches
  if isMatch then matchCountingDef event incrementBy count else pure Nothing

getDynamicTags :: LogEvent -> Metric -> LogIO [(Name, Text)]
getDynamicTags event Metric {mapTags, inheritTags} = do
  let inheritTags' = map (\n@(Name x) -> (n, x)) (fromMaybe [] inheritTags)
  let mapTags' = maybe [] HashMap.toList mapTags
  forM (mapTags' ++ inheritTags') $ \(tagk, field) -> do
    mStr <- matchTag field event
    let tagv = fromMaybe "null" mStr
    pure (tagk, tagv)

matchingMetrics :: LogEvent -> [Metric] -> LogIO [(CounterKey, Int64 -> Int64)]
matchingMetrics event metrics = do
  matches <- forM metrics $ \m -> do
    mF <- matchMetric event m
    let tags = maybe [] HashMap.toList (setTags m)
    dyntags <- getDynamicTags event m
    let key = CounterKey (name m) (tags ++ dyntags)
    pure ((key,) <$> mF)
  pure (catMaybes matches)

------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

handleLogEvent :: Config -> Counters -> [Metric] -> BL.ByteString -> LogIO ()
handleLogEvent Config { metricsMaxAge } counters metrics source =
  if BL.null source then say "Received empty source" else
    case Aeson.eitherDecode source of
      Left err -> say ("Aeson decode error: " <> Text.pack err)
      Right (Aeson.Object event) -> do
        matches <- matchingMetrics event metrics
        liftIO $ do
          timestamp <- getTimestamp
          mapM_ (bumpCounter metricsMaxAge timestamp counters) matches
      _ -> say "Received a non-object"

sourcesInBulk :: BL.ByteString -> LogIO [BL.ByteString]
sourcesInBulk body
  | BL.null body = do say "Received empty body"; pure []
  | otherwise = pure $ go (BL.split 0x0a body)
  where
    go [] = []
    go [_index] = []
    go (_index : source : objs) = source : go objs

processBulk :: Config -> Counters -> [Metric] -> BL.ByteString -> IO ()
processBulk config counters metrics body = do
  let bulk = mapM_ (handleLogEvent config counters metrics) =<< sourcesInBulk body
  logs <- execStateT bulk []
  mapM_ Text.putStrLn logs

server :: Config -> Counters -> ScottyM ()
server (config@Config {logHost, logPort, metrics}) counters = do
  Scotty.post "/_bulk" $ do
    body <- Scotty.body
    _ <- liftIO $ forkIO $ processBulk config counters metrics body
    let logUrl = "http://" ++ logHost ++ ":" ++ show logPort ++ "/_bulk"
    r <- liftIO $ Wreq.post logUrl body
    Scotty.setHeader "Content-Type" (toS (r ^. Wreq.responseHeader "Content-Type"))
    Scotty.raw (r ^. Wreq.responseBody)
  Scotty.matchAny "/" $ Scotty.text "" -- fluentd sends HEAD /

startServer :: Config -> Counters -> IO ()
startServer config counters = do
  sockets <- Systemd.getActivatedSockets
  app <- Scotty.scottyApp (server config counters)
  case sockets of
    Just (sock:_) -> Warp.runSettingsSocket Warp.defaultSettings sock app
    _ -> Warp.run (port config) app

------------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Loading config..."
  config <- loadConfig
  putStrLn "Finished loading config"
  counters <- Map.newIO
  _ <- forkIO (metricsThread config counters)
  startServer config counters
