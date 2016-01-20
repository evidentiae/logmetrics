{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Text (Text)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Web.Scotty as Scotty

data Config = Config
  { port :: Int
  , logHost :: Text
  , logPort :: Int
  , metricsHost :: Text
  , metricsPort :: Int
  , metricsInterval :: Int -- milliseconds
  } deriving (Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- ByteString.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Right config -> return config
    Left err -> error err

-- | OpenTSDB DataPoint
--
-- NOTE: the "No explicit implementation" warning is a GHC bug
data DataPoint = DataPoint
  { metric :: Text
  , timestamp :: Int64
  , value :: Int64
  , tags :: HashMap Text Text
  } deriving (Generic, ToJSON)

type Buffer = TVar [DataPoint]

postLogEvent :: Scotty.ActionM ()
postLogEvent = pure ()

server :: Config -> Buffer -> IO ()
server config _buffer =
  Scotty.scotty (port config) $ do
    Scotty.middleware logStdoutDev
    Scotty.post "/_bulk" $ do
      b <- Scotty.body
      liftIO $ print b
    Scotty.matchAny "/" $ Scotty.text "" -- fluentd sends HEAD /

metricsThread :: Config -> Buffer -> IO ()
metricsThread _config _buffer = pure ()

main :: IO ()
main = do
  config <- loadConfig
  buffer <- newTVarIO []
  _ <- forkIO (metricsThread config buffer)
  server config buffer
