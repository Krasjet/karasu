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

-- | Renders a preview HTML for markdown file.
renderPreview
  :: Text -- ^ the content of the markdown
  -> IO (Either PandocError Html) -- ^ error or the final HTML
renderPreview md = runIO $ do
  pandoc <- readMarkdown defKarasuReaderOptions md
  -- load preview templates
  res <- liftIO $ compileTemplateFile $ "templates" </> "preview" <.> "html"
  case res of
    Left e -> throwError $ PandocTemplateError (T.pack e)
    Right template -> do
      let wOpts = defKarasuWriterOptions { writerTemplate = Just template }
      writeHtml5 wOpts pandoc
