-- |
-- A few ultility functions to deal with runtime environments, i.e. a global
-- state shared by all handlers
module Karasu.Environment (KarasuEnv(..), lookupEnvVar, lookupEnvVarParse) where

import Karasu.Models (MasterPassword)

import Configuration.Dotenv.Environment (lookupEnv)
import Data.Maybe                       (fromMaybe)
import Database.Persist.Sqlite          (ConnectionPool)
import Network.Wai.Handler.Warp         (Port)
import Text.Read                        (readMaybe)

-- * Environment variables
data KarasuEnv
    = KarasuEnv
    { envDebug  :: Bool           -- in debug mode or not
    , envPool   :: ConnectionPool -- connection pool for database
    , envPort   :: Port           -- port number of the server
    , envMaster :: MasterPassword -- master password for creating documents (TODO encryption)
    , envDocDir :: FilePath       -- directory storing markdown files
    }

-- | Obtain an environment variable and parse with default value
lookupEnvVarParse
  :: Read a
  => String -- ^ the environemnt variable
  -> a      -- ^ the default value
  -> IO a
lookupEnvVarParse envVar defVal = do
  var <- lookupEnv envVar
  return $ fromMaybe defVal $ var >>= readMaybe

-- | Obtain an environment variable with default value
lookupEnvVar
  :: String -- ^ the environemnt variable
  -> String -- ^ the default value
  -> IO String
lookupEnvVar envVar defVal = do
  var <- lookupEnv envVar
  return $ fromMaybe defVal var
