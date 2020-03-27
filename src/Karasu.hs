module Karasu (runKarasu) where

import Karasu.Api
import Karasu.Database
import Karasu.Environment
import Karasu.Server

import qualified Data.Text as T

import Configuration.Dotenv                 (defaultConfig, loadFile)
import Control.Exception                    (bracket)
import Control.Monad                        (void, when)
import Data.Pool                            (destroyAllResources)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import System.Directory                     (doesFileExist)
import System.FilePath.Posix                ((</>))

-- * The actual main function
runKarasu :: IO ()
runKarasu = bracket loadEnv shutdownApp runApp

-- * application
app :: KarasuEnv -> Application
app env = logStdout $ serve karasuApi $ karasuServer env
--        ^ middleware for logging

-- | load runtime environments
loadEnv :: IO KarasuEnv
loadEnv = do
  -- load dotenv file if exists
  dotEnvExists <- doesFileExist ".env"
  when dotEnvExists $
    void $ loadFile defaultConfig
  debug  <- lookupEnvVarParse "KARASU_DEBUG" False
  port   <- lookupEnvVarParse "KARASU_PORT" 8080
  dbFile <- lookupEnvVar "KARASU_DB" $ "db" </> "karasu.db"
  poolSize <- lookupEnvVarParse "KARASU_POOL_SIZE" 5
  masterPass <- lookupEnvVar "KARASU_MASTERPASS" "karasu"
  pool   <- mkPool dbFile debug poolSize
  return KarasuEnv
    { envDebug  = debug
    , envPort   = port
    , envPool   = pool
    , envMaster = T.pack masterPass
    }

-- | start the server
runApp :: KarasuEnv -> IO ()
runApp env = runSettings settings $ app env
  where
  settings :: Settings
  settings =
    let port = envPort env in
    setPort port $
    setBeforeMainLoop (putStrLn $ "Karasu listening on port " ++ show port)
    defaultSettings

-- | clean up
shutdownApp :: KarasuEnv -> IO ()
shutdownApp env = destroyAllResources (envPool env)
