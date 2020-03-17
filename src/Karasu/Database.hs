{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions for querying database
module Karasu.Database
  ( doMigrations
  , runDb
  , mkPool
  , getDoc404WithMsg
  , getDoc404
  , getDocWithValidation
  , validateDoc
  ) where

import Karasu.Environment
import Karasu.Models

import qualified Data.Text as T

import Control.Monad           (void, when)
import Control.Monad.Except    (MonadError)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Logger    (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader    (MonadReader, asks)
import Data.ByteString.Lazy    (ByteString)
import Database.Persist.Sqlite
import Servant
import System.Directory        (createDirectoryIfMissing)
import System.FilePath         (takeDirectory)

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

-- | Run a single database transaction
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

-- | A simple wrapper for retrieving documents using DocId with error handling
-- and custom message
getDoc404WithMsg
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId              -- ^ the document id
  -> ByteString         -- ^ the message
  -> m (Entity DocInfo) -- ^ (docKey, docInfo)
getDoc404WithMsg docId msg = do
  res <- runDb $ getBy $ UniqueDocId docId
  case res of
    Nothing -> throwError err404 { errBody = msg }
    Just d  -> return d

-- | A simple wrapper for retrieving documents using DocId with error handling
getDoc404
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId              -- ^ the document id
  -> m (Entity DocInfo) -- ^ (docKey, docInfo)
getDoc404 docId = getDoc404WithMsg docId "Something wrong with the docId."

-- | getBy404 plus access code verification
getDocWithValidation
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId              -- ^ the document id
  -> Maybe AccessCode   -- ^ the access code
  -> m (Entity DocInfo) -- ^ (docKey, docInfo)
getDocWithValidation docId accCode = do
  d@(Entity _ doc) <- getDoc404 docId
  -- yes, no accessCode doesn't mean no protection
  when (docInfoAccCode doc /= accCode) $
    throwError err403 { errBody = "Access denied. Try again." }
  return d

-- | Only validate, ignore return value
validateDoc
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId            -- ^ the document id
  -> Maybe AccessCode -- ^ the access code
  -> m ()             -- ^ result ignored
validateDoc docId accCode = void $ getDocWithValidation docId accCode
