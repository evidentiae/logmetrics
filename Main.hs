{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Bifunctor
import Data.Char
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.String.Conv
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import GHC.Generics
import STMContainers.Map (Map)
import System.Environment
import System.IO
import Text.Read
import Web.Scotty (ScottyM)

import qualified Data.Aeson as Aeson
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
  } deriving (Generic, FromJSON, NFData)

type MetricName = Text

data Metric = Metric
  { name :: MetricName
  , matches :: [FieldMatch]
  , tags :: HashMap Text Text
  , tagsFromFields :: HashMap Text Text
  , incrementBy :: Maybe Text
  , count :: Maybe Text
  } deriving (Generic, FromJSON, NFData)

data Config = Config
  { port :: Int
  , logHost :: String
  , logPort :: Int
  , metricsHost :: String
  , metricsPort :: Int
  , metricsInterval :: Int -- milliseconds
  , metrics :: [Metric]
  } deriving (Generic, FromJSON, NFData)

validChar :: Char -> Char
validChar c
  | isAlpha c || isDigit c || c `elem` ['-', '_', '.', '/'] = c
  | otherwise = error ("Forbidden character in tag/metric name: " ++ [c])

validName :: Text -> Text
validName str = Text.map validChar str

validTags :: HashMap Text Text -> HashMap Text Text
validTags = HashMap.fromList . map (first validName) . HashMap.toList

validMetric :: Metric -> Metric
validMetric m =
  m { name = validName (name m)
    , tags = validTags (tags m)
    , tagsFromFields = validTags (tagsFromFields m)
    }

validConfig :: Config -> Config
validConfig c = c {metrics = map validMetric (metrics c)}

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- B.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Left err -> error err
    Right config -> pure $!! validConfig config

------------------------------------------------------------------------------
-- Log and metric types
------------------------------------------------------------------------------

type LogEvent = HashMap Text Aeson.Value

-- | OpenTSDB DataPoint
data DataPoint = DataPoint
  { dpMetric :: Text
  , dpTimestamp :: Int64
  , dpValue :: Int64
  , dpTags :: HashMap Text Text
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
  { ckMetricName :: MetricName
  , ckTagsFromFields :: [(Text, Text)]
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

counterToDataPoint :: HashMap MetricName Metric -> Int64 -> (CounterKey, Int64) -> DataPoint
counterToDataPoint metricsMap timestamp (key, count) =
  let metric = fromJust (HashMap.lookup (ckMetricName key) metricsMap) in
  DataPoint
    { dpMetric = name metric
    , dpTimestamp = timestamp
    , dpValue = count
    , dpTags = HashMap.union (tags metric) (HashMap.fromList (ckTagsFromFields key))
    }

sendMetrics :: Config -> HashMap MetricName Metric -> Counters -> IO ()
sendMetrics (Config {metricsHost, metricsPort}) metricsMap counters = do
  cs <- countersToList counters
  time <- getPOSIXTime
  let timestamp :: Int64 = round time -- seconds
  let points = map (counterToDataPoint metricsMap timestamp) cs
  when (not (null points)) $ do
    let url = "http://" ++ metricsHost ++ ":" ++ show metricsPort ++ "/api/put"
    void $ liftIO $ Wreq.post url (Aeson.encode points)

metricsThread :: Config -> Counters -> IO ()
metricsThread config counters = forever $ do
  threadDelay (metricsInterval config * 1000)
  forkIO (sendMetrics config metricsMap counters)
  where
    metricsMap = HashMap.fromList [(name m, m) | m <- metrics config]

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

matchMetric :: LogEvent -> Metric -> LogIO (Maybe (Int64 -> Int64))
matchMetric event (Metric {matches, incrementBy, count}) = do
  counterFun <- matchCountingDef event incrementBy count
  case counterFun of
    Nothing -> pure Nothing
    Just f -> do
      match <- anyM (matchField event) matches
      pure (if match then Just f else Nothing)

getTagsFromFields :: LogEvent -> Metric -> LogIO [(Text, Text)]
getTagsFromFields event (Metric {tagsFromFields}) =
  forM (HashMap.toList tagsFromFields) $ \(tagk, field) -> do
    mStr <- matchStringField field event
    let tagv = fromMaybe "null" mStr
    pure (tagk, tagv)

matchingMetrics :: LogEvent -> [Metric] -> LogIO [(CounterKey, Int64 -> Int64)]
matchingMetrics event metrics = do
  matches <- forM metrics $ \m -> do
    mF <- matchMetric event m
    tags <- getTagsFromFields event m
    let key = CounterKey (name m) tags
    pure ((key,) <$> mF)
  pure (catMaybes matches)

------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

handleLogEvent :: Counters -> [Metric] -> BL.ByteString -> LogIO ()
handleLogEvent counters metrics body = do
  if BL.null body then say "Received empty body" else
    case Aeson.eitherDecode body of
      Left err -> say ("Aeson decode error: " <> Text.pack err)
      Right (Aeson.Object event) -> do
        matches <- matchingMetrics event metrics
        liftIO $ mapM_ (bumpCounter counters) matches
      _ -> say "Received a non-object"

sourcesInBulk :: BL.ByteString -> [BL.ByteString]
sourcesInBulk body = go (BL.split 0x0a body)
  where
    go [] = []
    go (_index : []) = []
    go (_index : source : objs) = source : go objs

processBulk :: Counters -> [Metric] -> BL.ByteString -> IO ()
processBulk counters metrics body = do
  let bulk = mapM_ (handleLogEvent counters metrics) (sourcesInBulk body)
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
