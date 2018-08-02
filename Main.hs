{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey, (.=), (.:), (.:?))
import Data.Aeson.Types (FromJSONKeyFunction (FromJSONKeyText), toJSONKeyText)
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
import Data.Time
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
  , collectFrom :: Maybe (Text, Double)
  , collectConst :: Maybe Int64
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
      ((o .:? "collectFrom") >>= scaledField) <*>
      o .:? "collectConst"

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

mkTimestamp :: IO Int64
mkTimestamp = round <$> getPOSIXTime

------------------------------------------------------------------------------
-- Log and metric types
------------------------------------------------------------------------------

-- | OpenTSDB metric/tag name
newtype Name = Name Text
  deriving (Ord, Eq, Generic, Hashable)

validChar :: Char -> Aeson.Parser ()
validChar c
  | isAlpha c || isDigit c || c `elem` ['-', '_', '.', '/'] = pure ()
  | otherwise = fail ("Forbidden character in tag/metric name: " ++ [c])

validName :: Text -> Aeson.Parser Name
validName txt = mapM_ validChar (Text.unpack txt) >> pure (Name txt)

instance FromJSON Name where
  parseJSON = Aeson.withText "metric name" validName

-- Can't use GeneralizedNewtypeDeriving due to clash with DeriveAnyClass
instance FromJSONKey Name where
  fromJSONKey = FromJSONKeyText Name

-- Ditto
instance ToJSONKey Name where
  toJSONKey = toJSONKeyText (\(Name x) -> x)

instance ToJSON Name where
  toJSON (Name s) = Aeson.toJSON s

type LogEvent = HashMap Text Aeson.Value

-- | OpenTSDB DataPoint
data DataPoint = DataPoint
  { dpMetric :: Name
  , dpTimestamp :: Int64
  , dpValue :: Int64
  , dpTags :: HashMap Name Text
  } deriving (Eq, Generic)

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
bumpCounter :: Maybe Int64 -> Int64 -> Counters -> CounterKey -> (Int64 -> Int64) -> IO ()
bumpCounter metricsMaxAge timestamp counters key f = atomically $ do
  mCounter <- Map.lookup key counters
  case mCounter of
    Nothing -> Map.insert CounterValue {counterValue = f 0, lastChanged = timestamp} key counters
    Just CounterValue {counterValue,lastChanged} -> do
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
-- Buffer
------------------------------------------------------------------------------

type Buffer = MVar [(CounterKey, Int64, Int64)]

addToBuffer :: Buffer -> CounterKey -> Int64 -> [Int64] -> IO ()
addToBuffer buffer k t vs = modifyMVar_ buffer $ pure . ([(k,t,v) | v<-vs] ++)

------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

type Accumulators = (Counters, Buffer)

data Action
  = Count (Int64 -> Int64)
  | Collect Int64 [Int64] -- (timestamp, [value])

accumulate :: Config -> Int64 -> Accumulators -> CounterKey -> Action -> IO ()
accumulate config timestamp (counters, buffer) k = \case
  Count f -> bumpCounter (metricsMaxAge config) timestamp counters k f
  Collect t vs -> addToBuffer buffer k t vs

------------------------------------------------------------------------------
-- Metrics thread
------------------------------------------------------------------------------

toDataPoint :: Int64 -> (CounterKey, Int64) -> DataPoint
toDataPoint timestamp (CounterKey {metricName, tags}, counterValue) =
  DataPoint
    { dpMetric = metricName
    , dpTimestamp = timestamp
    , dpValue = counterValue
    , dpTags = HashMap.fromList tags
    }

countersToDataPoints :: Int64 -> Counters -> IO [DataPoint]
countersToDataPoints timestamp counters = do
  cs <- countersToList counters
  pure (map (\(k, CounterValue v _) -> toDataPoint timestamp (k, v)) cs)

bufferToDataPoints :: Buffer -> IO [DataPoint]
bufferToDataPoints buffer = do
  buf <- takeMVar buffer
  putMVar buffer []
  let cmp a b = mconcat
                  [ compare (dpMetric a) (dpMetric b)
                  , compare (dpTimestamp a) (dpTimestamp b)
                  , compare (dpValue a) (dpValue b)
                  ]
  pure (nub . sortBy cmp . map (\(k,t,v) -> toDataPoint t (k,v)) $ buf)

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

sendMetrics :: Config -> Accumulators -> IO ()
sendMetrics config (counters, buffer) = do
  timestamp <- mkTimestamp
  points <- (++) <$> countersToDataPoints timestamp counters <*>
                     bufferToDataPoints buffer
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

metricsThread :: Config -> Accumulators -> IO ()
metricsThread config accs = forever $ do
  threadDelay (metricsInterval config * 1000)
  forkIO (sendMetrics config accs)

------------------------------------------------------------------------------
-- Matching
------------------------------------------------------------------------------

-- Logging and IO, using StateT instead of the broken WriterT
type LogIO a = StateT [Text] IO a

say :: Text -> LogIO ()
say !txt = modify' (txt :)

matchError :: Show a => Text -> Text -> a -> LogIO ()
matchError msg key val =
  say (msg <> " Not matching. " <> toS (show key) <> ": " <> toS (show val))

matchTag :: Text -> LogEvent -> LogIO (Maybe Text)
matchTag key fields =
  case HashMap.lookup key fields of
    Nothing -> pure Nothing
    Just (Aeson.String str) -> pure (Just str)
    Just val -> do
      matchError "Non-string tags are not supported." key val
      pure Nothing

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
    Just val -> do
      matchError "Only string or number fields supported." field val
      pure False

-- Matches numbers or strings that can be converted to numbers
matchNumberishField :: Text -> LogEvent -> LogIO [Double]
matchNumberishField key event =
  case HashMap.lookup key event of
    Nothing -> pure []
    Just (Aeson.Number x) -> pure [toRealFloat x]
    Just (Aeson.String str) ->
      case readMaybe (toS str) of
        Nothing ->
          let strs = Text.splitOn ", " str
              nums = mapMaybe (readMaybe . toS) strs
          in if length nums == length strs then pure nums
             else do
               matchError "Couldn't parse string field as double or list of doubles." key str
               pure []
        Just x -> pure [x]
    Just val -> do
      matchError "Only string or number fields supported." key val
      pure []

parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp txt =
  let fmt = "%Y-%m-%dT%H:%M:%S%Q" in
  let parse suf = parseTimeM True defaultTimeLocale (fmt ++ suf) (toS txt) in
  case parse "%z" of
    Nothing -> parse "%Z"
    Just t -> Just t

getTimestamp :: LogEvent -> LogIO (Maybe Int64)
getTimestamp event =
  case HashMap.lookup "@timestamp" event of
    Nothing -> do
      say "No @timestamp field. Not matching."
      pure Nothing
    Just (Aeson.String txt)
      | Just t <- parseTimestamp txt -> pure (Just (round (utcTimeToPOSIXSeconds t)))
      | otherwise -> do
          matchError "Parse failure." "@timestamp" txt
          pure Nothing
    Just val -> do
      matchError "Non-string @timestamp field." "@timestamp" val
      pure Nothing

matchCountingDef :: LogEvent -> Metric -> LogIO (Maybe Action)
matchCountingDef event metric = do
  let Metric {incrementBy, collectFrom, collectConst} = metric
  case (incrementBy, collectFrom, collectConst) of
    (Nothing, Nothing, Nothing) -> pure (Just (Count (+1)))
    (Just (field, mul), _, _) -> do
      ns <- matchNumberishField field event
      pure $ if null ns then Nothing
             else Just (Count (round (mul * sum ns) +))
    (Nothing, Just (field, mul), _) -> do
      timestamp <- getTimestamp event
      case timestamp of
        Nothing -> pure Nothing
        Just t -> do
          ns <- matchNumberishField field event
          pure $ if null ns then Nothing
                 else Just (Collect t (map (round . (mul*)) ns))
    (Nothing, Nothing, Just x) -> do
      timestamp <- getTimestamp event
      case timestamp of
        Nothing -> pure Nothing
        Just t -> pure (Just (Collect t [x]))

matchRec :: LogEvent -> Match -> LogIO Bool
matchRec event (MatchField m) = matchField event m
matchRec event (MatchAny ms) = anyM (matchRec event) ms
matchRec event (MatchAll ms) = allM (matchRec event) ms

matchMetric :: LogEvent -> Metric -> LogIO (Maybe Action)
matchMetric event metric = do
  isMatch <- matchRec event (matches metric)
  if isMatch then matchCountingDef event metric else pure Nothing

getDynamicTags :: LogEvent -> Metric -> LogIO [(Name, Text)]
getDynamicTags event Metric {mapTags, inheritTags} = do
  let inheritTags' = map (\n@(Name x) -> (n, x)) (fromMaybe [] inheritTags)
  let mapTags' = maybe [] HashMap.toList mapTags
  forM (mapTags' ++ inheritTags') $ \(tagk, field) -> do
    mStr <- matchTag field event
    let tagv = fromMaybe "null" mStr
    pure (tagk, tagv)

matchingMetrics :: LogEvent -> [Metric] -> LogIO [(CounterKey, Action)]
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

handleLogEvent :: Config -> Accumulators -> [Metric] -> BL.ByteString -> LogIO ()
handleLogEvent config accs metrics source =
  if BL.null source then say "Received empty source" else
    case Aeson.eitherDecode source of
      Left err -> say ("Aeson decode error: " <> Text.pack err)
      Right (Aeson.Object event) -> do
        matches <- matchingMetrics event metrics
        liftIO $ do
          timestamp <- mkTimestamp
          mapM_ (uncurry $ accumulate config timestamp accs) matches
      _ -> say "Received a non-object"

sourcesInBulk :: BL.ByteString -> LogIO [BL.ByteString]
sourcesInBulk body
  | BL.null body = do say "Received empty body"; pure []
  | otherwise = pure $ go (BL.split 0x0a body)
  where
    go [] = []
    go [_index] = []
    go (_index : source : objs) = source : go objs

processBulk :: Config -> Accumulators -> [Metric] -> BL.ByteString -> IO ()
processBulk config accs metrics body = do
  let bulk = mapM_ (handleLogEvent config accs metrics) =<< sourcesInBulk body
  logs <- execStateT bulk []
  mapM_ Text.putStrLn logs

server :: Config -> Accumulators -> ScottyM ()
server (config@Config {logHost, logPort, metrics}) accs = do
  Scotty.post "/_bulk" $ do
    body <- Scotty.body
    contentType <- Scotty.header "Content-Type"
    _ <- liftIO $ forkIO $ processBulk config accs metrics body
    let logUrl = "http://" ++ logHost ++ ":" ++ show logPort ++ "/_bulk"
        contentType' = fromMaybe "application/json" contentType
        wreqOpts = Wreq.defaults & Wreq.header "Content-Type" .~ [toS contentType']
    r <- liftIO $ Wreq.postWith wreqOpts logUrl body
    Scotty.setHeader "Content-Type" (toS (r ^. Wreq.responseHeader "Content-Type"))
    Scotty.raw (r ^. Wreq.responseBody)
  Scotty.matchAny "/" $ Scotty.text "" -- fluentd sends HEAD /

startServer :: Config -> Accumulators -> IO ()
startServer config accs = do
  sockets <- Systemd.getActivatedSockets
  app <- Scotty.scottyApp (server config accs)
  case sockets of
    Just (sock:_) -> Warp.runSettingsSocket Warp.defaultSettings sock app
    _ -> Warp.run (port config) app

------------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Loading config..."
  config <- loadConfig
  putStrLn "Finished loading config"
  counters <- Map.newIO
  buffer <- newMVar []
  let accs = (counters, buffer)
  _ <- forkIO (metricsThread config accs)
  startServer config accs
