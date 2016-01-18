{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Web.Scotty as Scotty

data Config = Config
  { logHost :: Text
  , logPort :: Int
  , metricsHost :: Text
  , metricsPort :: Int
  , port :: Int
  } deriving (Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- ByteString.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Right config -> return config
    Left err -> error err

postLogEvent :: Scotty.ActionM ()
postLogEvent = pure ()

server :: Scotty.ScottyM ()
server = Scotty.post "/log/event" postLogEvent

main :: IO ()
main = do
  config <- loadConfig
  Scotty.scotty (port config) server
