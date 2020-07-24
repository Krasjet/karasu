{-# LANGUAGE FlexibleContexts #-}

-- | Karasu side pandoc functions
module Karasu.Pandoc (regenPreview) where

import Karasu.Environment
import Karasu.Models
import Karasu.Pandoc.Renderer

import qualified Data.ByteString.Lazy.Char8 as LB8

import Control.Exception      (catch, throwIO)
import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (MonadReader)
import Servant
import System.Directory
import System.FilePath        ((<.>), (</>))
import System.IO.Error        (isDoesNotExistError)

-- | Regenerate the rendered html in the view directory.
--
-- This function will make sure the old html in the view directory gets
-- flushed.
--
-- A new html will be rendered and returned, but it will not be saved
-- to view directly because in rare occasions it can make the rendered
-- html out of sync with the latest markdown.
--
-- Consider
--   ver 1                          ver 2
--   update md
--                                  update md
--                                  save html
--   save html
-- now the old version (ver 1) is saved to disk but the latest version is
-- ver 2.
--
-- Instead of creating a lock for each document, we will reuse the lock from
-- database and only save the html to disk when the /view/ endpoint gets
-- queried. This will make sure the served html is always up-to-date.
regenPreview
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId         -- ^ the document id
  -> HTMLTemplate  -- ^ template
  -> Markdown      -- ^ the markdown document
  -> m EscapedHtml -- ^ rendered html
regenPreview docId tmpl md = do
  out <- liftIO $ renderDisplay docId tmpl md
  case out of
    Left err -> throwError err400 { errBody = LB8.pack $ show err }
    Right h  -> do
      -- remove the old version
      -- the new version will be generated when the /view/ endpoint is accessed
      let fname = "view" </> docId </> "index" <.> "html"
      liftIO $ removeFile fname `catch` ignoreEmpty
      return h
  where
    ignoreEmpty :: IOError -> IO ()
    ignoreEmpty e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
