{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.String.Conv
import Data.Text (Text)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Network.Wreq as Wreq
import qualified Web.Scotty as Scotty

data Config = Config
  { port :: Int
  , logHost :: String
  , logPort :: Int
  , metricsHost :: String
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

server :: Config -> Buffer -> IO ()
server (Config {port, logHost, logPort})  _buffer =
  Scotty.scotty port $ do
    Scotty.middleware logStdoutDev
    Scotty.post "/_bulk" $ do
      b <- Scotty.body
      let url = "http://" ++ logHost ++ ":" ++ show logPort ++ "/_bulk"
      r <- liftIO $ Wreq.post url b
      Scotty.setHeader "Content-Type" (toS (r ^. Wreq.responseHeader "Content-Type"))
      Scotty.raw (r ^. Wreq.responseBody)
    Scotty.matchAny "/" $ Scotty.text "" -- fluentd sends HEAD /

metricsThread :: Config -> Buffer -> IO ()
metricsThread _config _buffer = pure ()

main :: IO ()
main = do
  config <- loadConfig
  buffer <- newTVarIO []
  _ <- forkIO (metricsThread config buffer)
  server config buffer
