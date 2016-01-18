{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString

data Config = Config
  { logHost :: Text
  , logPort :: Integer
  , metricsHost :: Text
  , metricsPort :: Text
  } deriving (Generic, FromJSON)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "LOGMETRICS_CONFIG"
  json <- ByteString.readFile configPath
  case Aeson.eitherDecodeStrict json of
    Right config -> return config
    Left err -> error err

main :: IO ()
main = do
  _config <- loadConfig
  pure ()
  {-
  t <- newIORef (Trie.empty, IntMap.empty)
  _ <- forkIO (loadThings thingdir t)
  app <- application t
  exePath <- getExecutablePath
  let pkgPath = takeDirectory (takeDirectory exePath)
  let certPath = pkgPath </> "certificate.pem"
  let keyPath = pkgPath </> "key.pem"
  let settings = tlsSettings certPath keyPath
  runTLS settings (setPort 443 defaultSettings) app
  -}
