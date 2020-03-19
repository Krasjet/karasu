{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions for querying database
module Karasu.Database
  ( doMigrations
  , runDb
  , mkPool
  , getDoc404WithMsg
  , getDoc404
  , docExists404
  , getDocWithValidation
  , validateDoc
  ) where

import Karasu.Environment
import Karasu.Models
import Karasu.Utils

import qualified Data.Text as T

import Control.Monad           (void, when)
import Control.Monad.Except    (MonadError)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Logger    (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader    (MonadReader, asks)
import Data.ByteString.Lazy    (ByteString)
import Database.Persist.Sqlite
import Servant

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

-- | Run a single database transaction
runDb :: (MonadReader KarasuEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks envPool
  liftIO $ runSqlPool query pool

-- | Create databse pool
mkPool :: FilePath -> Bool -> Int -> IO ConnectionPool
mkPool dbFile debug poolSize = do
  createParentDir dbFile
  pool <- if debug
            then runStdoutLoggingT $ createSqlitePool (T.pack dbFile) poolSize
            else runNoLoggingT $ createSqlitePool (T.pack dbFile) poolSize
  runSqlPool doMigrations pool
  return pool

-- | A simple wrapper for retrieving documents using DocId with error handling
-- and custom message
getDoc404WithMsg
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId                    -- ^ the document id
  -> ByteString               -- ^ the message
  -> m (Key DocInfo, DocInfo) -- ^ (docKey, docInfo)
getDoc404WithMsg docId msg = do
  res <- runDb $ getBy $ UniqueDocId docId
  case res of
    Nothing -> throwError err404 { errBody = msg }
    Just (Entity docKey doc) -> return (docKey, doc)

-- | A simple wrapper for retrieving documents using DocId with error handling
getDoc404
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId                    -- ^ the document id
  -> m (Key DocInfo, DocInfo) -- ^ (docKey, docInfo)
getDoc404 = flip getDoc404WithMsg "Something wrong with the docId."

-- | Only check if the document exists or not. Raise 404 error if not.
docExists404
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId -- ^ the document id
  -> m ()  -- ^ result ignored
docExists404 = void . flip getDoc404WithMsg "Nothing here."

-- | getBy404 plus access code verification
getDocWithValidation
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId                    -- ^ the document id
  -> Maybe AccessCode         -- ^ the access code
  -> m (Key DocInfo, DocInfo) -- ^ (docKey, docInfo)
getDocWithValidation docId accCode = do
  (docKey, doc) <- getDoc404 docId
  -- yes, no accessCode doesn't mean no protection
  when (docInfoAccCode doc /= accCode) $
    throwError err403 { errBody = "Access denied. Try again." }
  return (docKey, doc)

-- | Only validate, ignore return value
validateDoc
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId            -- ^ the document id
  -> Maybe AccessCode -- ^ the access code
  -> m ()             -- ^ result ignored
validateDoc = (void .) . getDocWithValidation
