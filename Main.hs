{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Aeson (FromJSON, ToJSON, (.=), (.:), (.:?))
import Data.Char
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.String.Conv
import Data.Text (Text)
import Data.Time.Clock.POSIX
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified ListT as ListT
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import qualified STMContainers.Map as Map
import qualified System.Systemd.Daemon as Systemd
import qualified Web.Scotty as Scotty

------------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------------

data FieldMatch = FieldMatch
  { match :: Text -- match type: "exact" or "contains"
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
  , incrementBy :: Maybe Text
  , count :: Maybe Text
  } deriving Generic

instance FromJSON Metric where
  parseJSON = Aeson.withObject "metric" $ \o ->
    let
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
      o .:? "incrementBy" <*>
      o .:? "count"

data Config = Config
  { port :: Int
  , logHost :: String
  , logPort :: Int
  , metricsHost :: String
  , metricsPort :: Int
  , metricsInterval :: Int -- milliseconds
  , metrics :: [Metric]
  } deriving (Generic, FromJSON)

validateMetric :: Metric -> IO ()
validateMetric (Metric {setTags, mapTags, inheritTags}) =
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
  toEncoding (Name s) = Aeson.toEncoding s

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
  toEncoding (DataPoint {dpMetric, dpTimestamp, dpValue, dpTags}) =
    Aeson.pairs $
      "metric" .= dpMetric <>
      "timestamp" .= dpTimestamp <>
      "value" .= dpValue <>
      "tags" .= dpTags

------------------------------------------------------------------------------
-- Counters
------------------------------------------------------------------------------

data CounterKey = CounterKey
  { metricName :: Name
  , tags :: [(Name, Text)]
  } deriving (Generic, Hashable, Eq)

type Counters = Map CounterKey Int64

-- Creates a new counter on-the-fly if none is found
bumpCounter :: Counters -> (CounterKey, Int64 -> Int64) -> IO ()
bumpCounter counters (key, f) = atomically $ do
  mCounter <- Map.lookup key counters
  case mCounter of
    Nothing -> Map.insert (f 0) key counters
    Just counter -> Map.insert (f counter) key counters

countersToList :: Counters -> IO [(CounterKey, Int64)]
countersToList counters = atomically (ListT.toList (Map.stream counters))

------------------------------------------------------------------------------
-- Metrics thread
------------------------------------------------------------------------------

counterToDataPoint :: Int64 -> (CounterKey, Int64) -> DataPoint
counterToDataPoint timestamp (CounterKey {metricName, tags}, dpValue) =
  DataPoint
    { dpMetric = metricName
    , dpTimestamp = timestamp
    , dpValue
    , dpTags = HashMap.fromList tags
    }

sendMetrics :: Config -> Counters -> IO ()
sendMetrics (Config {metricsHost, metricsPort}) counters = do
  cs <- countersToList counters
  time <- getPOSIXTime
  let timestamp :: Int64 = round time -- seconds
  let points = map (counterToDataPoint timestamp) cs
  when (not (null points)) $ do
    let url = "http://" ++ metricsHost ++ ":" ++ show metricsPort ++ "/api/put"
    void $ liftIO $ Wreq.post url (Aeson.encode points)

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

matchStringField :: Text -> LogEvent -> LogIO (Maybe Text)
matchStringField key fields =
  case HashMap.lookup key fields of
    Nothing -> pure Nothing
    Just (Aeson.String str) -> pure (Just str)
    Just val -> matchError "Non-string fields not supported." key val

matchIntStringField :: Text -> LogEvent -> LogIO (Maybe Int64)
matchIntStringField key event = do
  mStr <- matchStringField key event
  case mStr of
    Nothing -> pure Nothing
    Just str ->
      case readMaybe (toS str) of
        Nothing -> matchError "Couldn't parse string field as integer." key str
        n -> pure n

matchField :: LogEvent -> FieldMatch -> LogIO Bool
matchField event (FieldMatch {match, field, value}) = do
  mStr <- matchStringField field event
  case mStr of
    Nothing -> pure False
    Just str -> pure $
      case match of
        "exact" -> value == str
        "contains" -> value `Text.isInfixOf` str
        _ -> error "unsupported match type"

matchCountingDef :: LogEvent -> Maybe Text -> Maybe Text -> LogIO (Maybe (Int64 -> Int64))
matchCountingDef event incrementBy count =
  case (incrementBy, count) of
    (Nothing, Nothing) -> pure (Just (+1))
    (Just field, _) -> apply (+) field
    (Nothing, Just field) -> apply const field
  where
    apply f field = do
      mN <- matchIntStringField field event
      case mN of
        Nothing -> pure Nothing
        Just n -> pure (Just (f n))

matchRec :: LogEvent -> Match -> LogIO Bool
matchRec event (MatchField m) = matchField event m
matchRec event (MatchAny ms) = anyM (matchRec event) ms
matchRec event (MatchAll ms) = allM (matchRec event) ms

matchMetric :: LogEvent -> Metric -> LogIO (Maybe (Int64 -> Int64))
matchMetric event (Metric {matches, incrementBy, count}) = do
  counterFun <- matchCountingDef event incrementBy count
  case counterFun of
    Nothing -> pure Nothing
    Just f -> do
      b <- matchRec event matches
      pure (if b then Just f else Nothing)

getDynamicTags :: LogEvent -> Metric -> LogIO [(Name, Text)]
getDynamicTags event (Metric {mapTags, inheritTags}) = do
  let inheritTags' = map (\n@(Name x) -> (n, x)) (fromMaybe [] inheritTags)
  let mapTags' = maybe [] HashMap.toList mapTags
  forM (mapTags' ++ inheritTags') $ \(tagk, field) -> do
    mStr <- matchStringField field event
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

handleLogEvent :: Counters -> [Metric] -> BL.ByteString -> LogIO ()
handleLogEvent counters metrics source = do
  if BL.null source then say "Received empty source" else
    case Aeson.eitherDecode source of
      Left err -> say ("Aeson decode error: " <> Text.pack err)
      Right (Aeson.Object event) -> do
        matches <- matchingMetrics event metrics
        liftIO $ mapM_ (bumpCounter counters) matches
      _ -> say "Received a non-object"

sourcesInBulk :: BL.ByteString -> LogIO [BL.ByteString]
sourcesInBulk body
  | BL.null body = do say "Received empty body"; pure []
  | otherwise = pure $ go (BL.split 0x0a body)
  where
    go [] = []
    go (_index : []) = []
    go (_index : source : objs) = source : go objs

processBulk :: Counters -> [Metric] -> BL.ByteString -> IO ()
processBulk counters metrics body = do
  let bulk = mapM_ (handleLogEvent counters metrics) =<< sourcesInBulk body
  logs <- execStateT bulk []
  mapM_ Text.putStrLn logs

server :: Config -> Counters -> ScottyM ()
server (Config {logHost, logPort, metrics}) counters = do
  Scotty.post "/_bulk" $ do
    body <- Scotty.body
    _ <- liftIO $ forkIO $ (processBulk counters metrics body)
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
