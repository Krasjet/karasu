{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions for querying database
module Karasu.Database (doMigrations, runDb, mkPool) where

import Karasu.Environment
import Karasu.Models

import qualified Data.Text as T

import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Logger    (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader    (MonadReader, asks)
import Database.Persist.Sqlite
import System.Directory        (createDirectoryIfMissing)
import System.FilePath         (takeDirectory)

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader KarasuEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks envPool
  liftIO $ runSqlPool query pool

-- | Create databse pool
mkPool :: FilePath -> Bool -> IO ConnectionPool
mkPool dbFile debug = do
  createDirectoryIfMissing True $ takeDirectory dbFile
  pool <- if debug
            then runStdoutLoggingT $ createSqlitePool (T.pack dbFile) 5
            else runNoLoggingT $ createSqlitePool (T.pack dbFile) 5
  runSqlPool doMigrations pool
  return pool
