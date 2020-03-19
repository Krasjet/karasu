{-# LANGUAGE OverloadedStrings #-}

-- | The renderer for markdown -> html conversion
module Karasu.Pandoc.Renderer (renderPreview, renderDisplay) where

import Karasu.Pandoc.Options
import Karasu.Pandoc.Filters

import qualified Data.Text as T

import Control.Monad.Except   (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import System.FilePath        ((<.>), (</>))
import Text.Blaze.Html
import Text.DocTemplates
import Text.Pandoc

-- | Render a preview HTML from markdown file.
renderPreview
  :: Text                         -- ^ content of the markdown
  -> IO (Either PandocError Html) -- ^ error or the final HTML
renderPreview = renderWith writeHtml5 functionalFilters

-- | Render the final html for saving from markdown file.
renderDisplay
  :: Text                         -- ^ content of the markdown
  -> IO (Either PandocError Text) -- ^ error or the final text
renderDisplay = renderWith writeHtml5String (functionalFilters <> cosmeticFilters)

renderWith
  :: (WriterOptions -> Pandoc -> PandocIO a) -- ^ writer
  -> [PandocFilterIO]                        -- ^ a list of pandoc filters
  -> Text                                    -- ^ content of the markdown
  -> IO (Either PandocError a)               -- ^ error or the final HTML
renderWith writer fs md = runIO $ do
  pandoc' <- readMarkdown defKarasuReaderOptions md
  -- apply filters
  pandoc <- applyPandocFiltersIO fs pandoc'
  -- load preview templates
  res <- liftIO $ compileTemplateFile $ "templates" </> "preview" <.> "html"
  case res of
    Left e -> throwError $ PandocTemplateError (T.pack e)
    Right template -> do
      let wOpts = defKarasuWriterOptions { writerTemplate = Just template }
      writer wOpts pandoc
