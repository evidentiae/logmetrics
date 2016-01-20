{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Aeson (FromJSON, ToJSON, (.=))
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
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import STMContainers.Map (Map)
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified ListT as ListT
import qualified Network.Wreq as Wreq
import qualified STMContainers.Map as Map
import qualified Web.Scotty as Scotty

------------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------------

data FieldMatch = FieldMatch
  { match :: Text -- match type: "exact" or "contains"
  , field :: Text
  , value :: Text
  } deriving (Generic, FromJSON)

type MetricName = Text

data Metric = Metric
  { name :: MetricName
  , matches :: [FieldMatch]
  , tags :: HashMap Text Text
  , tagsFromFields :: HashMap Text Text
  } deriving (Generic, FromJSON)

data Config = Config
  { port :: Int
  , logHost :: String
  , logPort :: Int
  , metricsHost :: String
  , metricsPort :: Int
  , metricsInterval :: Int -- milliseconds
  , metrics :: [Metric]
  } deriving (Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- B.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Left err -> error err
    Right config -> return config

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
-- Counters
------------------------------------------------------------------------------

data CounterKey = CounterKey
  { ckMetricName :: MetricName
  , ckTagsFromFields :: [(Text, Text)]
  } deriving (Generic, Hashable, Eq)

type Counters = Map CounterKey Int64

-- Creates a new counter on-the-fly if none is found
bumpCounter :: Counters -> CounterKey -> IO ()
bumpCounter counters key = atomically $ do
  mCounter <- Map.lookup key counters
  case mCounter of
    Nothing -> Map.insert 0 key counters
    Just counter -> Map.insert (counter + 1) key counters

countersToList :: Counters -> IO [(CounterKey, Int64)]
countersToList counters = atomically (ListT.toList (Map.stream counters))

------------------------------------------------------------------------------
-- Matching
------------------------------------------------------------------------------

lookupString :: Text -> HashMap Text Aeson.Value -> Maybe Text
lookupString key fields =
  case HashMap.lookup key fields of
    Nothing -> Nothing
    Just (Aeson.String str) -> Just str
    _ -> error "we only support matching/tagging on string fields"

matchField :: LogEvent -> FieldMatch -> Bool
matchField event (FieldMatch {match, field, value}) =
  case lookupString field event of
    Nothing -> False
    Just str ->
      case match of
        "exact" -> value == str
        "contains" -> value `Text.isInfixOf` str
        _ -> error "unsupported match type"

matchMetric :: LogEvent -> Metric -> Bool
matchMetric event (Metric {matches}) = any (matchField event) matches

getTagsFromFields :: LogEvent -> Metric -> [(Text, Text)]
getTagsFromFields event (Metric {tagsFromFields}) =
  [ (tag, fromMaybe "null" (lookupString field event))
  | (tag, field) <- HashMap.toList tagsFromFields ]

matchingMetrics :: LogEvent -> [Metric] -> [CounterKey]
matchingMetrics event metrics =
  [ CounterKey (name m) (getTagsFromFields event m)
  | m <- metrics, matchMetric event m ]

------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

sourcesInBulk :: BL.ByteString -> [BL.ByteString]
sourcesInBulk body = go (BL.split 0x0a body)
  where
    go [] = []
    go (_index : []) = []
    go (_index : source : objs) = source : go objs

handleBulk :: Counters -> [Metric] -> BL.ByteString -> IO ()
handleBulk counters metrics body =
  mapM_ (handleLogEvent counters metrics) (sourcesInBulk body)

handleLogEvent :: Counters -> [Metric] -> BL.ByteString -> IO ()
handleLogEvent counters metrics body = do
  if BL.null body then putStrLn "Received empty body" else
    case Aeson.eitherDecode body of
      Left err -> putStrLn err
      Right (Aeson.Object event) -> do
        let keys = matchingMetrics event metrics
        mapM_ (bumpCounter counters) keys
      _ -> putStrLn "Received a non-object"

server :: Config -> Counters -> IO ()
server (Config {port, logHost, logPort, metrics}) counters =
  let logUrl = "http://" ++ logHost ++ ":" ++ show logPort ++ "/_bulk" in
  Scotty.scotty port $ do
    Scotty.middleware logStdoutDev
    Scotty.post "/_bulk" $ do
      body <- Scotty.body
      _ <- liftIO $ forkIO $ (handleBulk counters metrics body)
      r <- liftIO $ Wreq.post logUrl body
      Scotty.setHeader "Content-Type" (toS (r ^. Wreq.responseHeader "Content-Type"))
      Scotty.raw (r ^. Wreq.responseBody)
    Scotty.matchAny "/" $ Scotty.text "" -- fluentd sends HEAD /

------------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Loading config..."
  config <- loadConfig
  putStrLn "Finished loading config"
  counters <- Map.newIO
  _ <- forkIO (metricsThread config counters)
  server config counters
