{-# LANGUAGE FlexibleContexts  #-}

-- | Karasu side pandoc functions
module Karasu.Pandoc (renderSaveMarkdownPreview) where

import Karasu.Environment
import Karasu.Models
import Karasu.Pandoc.Renderer
import Karasu.Utils

import qualified Data.ByteString.Lazy.Char8 as LB8

import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (MonadReader)
import Data.Text              (Text)
import Servant
import System.FilePath        ((<.>), (</>))

-- | Render and save markdown preview to disk
renderSaveMarkdownPreview
  :: (MonadReader KarasuEnv m, MonadIO m, MonadError ServerError m)
  => DocId    -- ^ the document id
  -> Markdown -- ^ the markdown document
  -> m Text   -- ^ rendered html
renderSaveMarkdownPreview docId md = do
  out <- liftIO $ renderDisplay docId md
  case out of
    Left err -> throwError err400 { errBody = LB8.pack $ show err }
    Right h  -> do
      let htmlFile = "view" </> docId </> "index" <.> "html"
      liftIO $ writeFileHandleMissing' htmlFile h
      return h
