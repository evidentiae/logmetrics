{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Hashable
import Data.Maybe
import Data.String.Conv
import Data.Text (Text)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
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
-- Data points
------------------------------------------------------------------------------

-- | OpenTSDB DataPoint
--
-- NOTE: the "No explicit implementation" warning is a GHC bug
data DataPoint = DataPoint
  { dpMetric :: Text
  , dpTimestamp :: Int64
  , dpValue :: Int64
  , dpTags :: HashMap Text Text
  } deriving (Generic, ToJSON)

type Buffer = TVar [DataPoint]

------------------------------------------------------------------------------
-- Metrics thread
------------------------------------------------------------------------------

metricsThread :: Config -> Buffer -> IO ()
metricsThread _config _buffer = pure ()

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

------------------------------------------------------------------------------
-- LogEvent
------------------------------------------------------------------------------

data LogEvent = LogEvent
  { timestamp :: Text
  , fields :: HashMap Text Aeson.Value
  } deriving Show

instance FromJSON LogEvent where
  parseJSON (Aeson.Object o) = LogEvent <$> o .: "@timestamp" <*> pure o
  parseJSON _ = mzero

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
matchField (LogEvent {fields}) (FieldMatch {match, field, value}) =
  case lookupString field fields of
    Nothing -> False
    Just str ->
      case match of
        "exact" -> value == str
        "contains" -> value `Text.isInfixOf` str
        _ -> error "unsupported match type"

matchMetric :: LogEvent -> Metric -> Bool
matchMetric event (Metric {matches}) = any (matchField event) matches

getTagsFromFields :: LogEvent -> Metric -> [(Text, Text)]
getTagsFromFields (LogEvent {fields}) (Metric {tagsFromFields}) =
  [ (tag, fromMaybe "null" (lookupString field fields))
  | (tag, field) <- HashMap.toList tagsFromFields ]

matchingMetrics :: LogEvent -> [Metric] -> [CounterKey]
matchingMetrics event metrics =
  [ CounterKey (name m) (getTagsFromFields event m)
  | m <- metrics, matchMetric event m ]

------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

handleLogEvent :: Buffer -> Counters -> [Metric] -> BL.ByteString -> IO ()
handleLogEvent _buffer counters metrics body =
  case Aeson.eitherDecode body of
    Left err -> putStrLn err
    Right (event :: LogEvent) -> do
      let keys = matchingMetrics event metrics
      mapM_ (bumpCounter counters) keys

server :: Config -> Buffer -> Counters -> IO ()
server (Config {port, logHost, logPort, metrics}) buffer counters =
  let logUrl = "http://" ++ logHost ++ ":" ++ show logPort ++ "/_bulk" in
  Scotty.scotty port $ do
    Scotty.middleware logStdoutDev
    Scotty.post "/_bulk" $ do
      body <- Scotty.body
      _ <- liftIO $ forkIO $ (handleLogEvent buffer counters metrics body)
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
  buffer <- newTVarIO []
  _ <- forkIO (metricsThread config buffer)
  counters <- Map.newIO
  server config buffer counters
