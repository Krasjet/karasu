{-# LANGUAGE OverloadedStrings #-}

-- | The renderer for markdown -> html conversion
module Karasu.Pandoc.Renderer (renderPreview) where

import Karasu.Pandoc.Options

import qualified Data.Text as T

import Control.Monad.Except   (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import System.FilePath        ((<.>), (</>))
import Text.Blaze.Html
import Text.DocTemplates
import Text.Pandoc

renderPreview :: Text -> IO (Either PandocError Html)
renderPreview md = runIO $ do
  pandoc <- readMarkdown defKarasuReaderOptions md
  res <- liftIO $ compileTemplateFile $ "templates" </> "preview" <.> "html"
  case res of
    Left e -> throwError $ PandocTemplateError (T.pack e)
    Right template -> do
      let wOpts = defKarasuWriterOptions { writerTemplate = Just template }
      writeHtml5 wOpts pandoc
