module Karasu (runKarasu) where

import Karasu.Api
import Karasu.Database
import Karasu.Environment
import Karasu.Server

import qualified Data.Text as T

import Configuration.Dotenv                 (defaultConfig, loadFile)
import Control.Monad                        (void)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import System.FilePath.Posix                ((</>))

-- * application
app :: KarasuEnv -> Application
app env = serve karasuApi $ karasuServer env

-- * server
karasuSettings :: KarasuEnv -> Settings
karasuSettings env =
  let port = envPort env in
  setPort port $
  setBeforeMainLoop (putStrLn $ "Karasu listening on port " ++ show port)
  defaultSettings

-- | The actual main function
runKarasu :: IO ()
runKarasu = do
  -- load environments
  env <- loadEnv
  let settings = karasuSettings env
  runSettings settings $ logStdout $ app env
  --                     ^ middleware for logging

-- | Load runtime environments
loadEnv :: IO KarasuEnv
loadEnv = do
  void $ loadFile defaultConfig
  debug  <- lookupEnvVarParse "KARASU_DEBUG" False
  port   <- lookupEnvVarParse "KARASU_PORT" 8080
  dbFile <- lookupEnvVar "KARASU_DB" $ "db" </> "karasu.db"
  masterPass <- lookupEnvVar "KARASU_MASTERPASS" "karasu"
  docDir <- lookupEnvVar "KARASU_DOCDIR" "docs"
  pool   <- mkPool dbFile debug
  return KarasuEnv
    { envDebug  = debug
    , envPort   = port
    , envPool   = pool
    , envMaster = T.pack masterPass
    , envDocDir = docDir
    }
