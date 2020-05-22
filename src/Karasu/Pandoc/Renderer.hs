{-# LANGUAGE OverloadedStrings #-}

-- | The renderer for markdown -> html conversion
module Karasu.Pandoc.Renderer (renderPreview, renderDisplay, HTMLTemplate(..)) where

import Karasu.Models
import Karasu.Pandoc.Filters
import Karasu.Pandoc.Options

import qualified Data.Text as T

import Control.Monad.Except     (throwError)
import Control.Monad.IO.Class   (liftIO)
import Data.Text                (Text)
import Text.Blaze.Html
import Text.DocTemplates
import Text.Pandoc
import Text.Pandoc.Filter.Utils

-- | data type for an html template
data HTMLTemplate = HTMLTemplate FilePath Text

-- | Render a preview HTML from markdown file.
renderPreview
  :: DocId                        -- ^ document id
  -> HTMLTemplate                 -- ^ template
  -> Text                         -- ^ content of the markdown file
  -> IO (Either PandocError Html) -- ^ error or the final HTML
renderPreview = renderWith writeHtml5 . functionalFilters

-- | Render the final html for saving from markdown file.
renderDisplay
  :: DocId                        -- ^ document id
  -> HTMLTemplate                 -- ^ template
  -> Text                         -- ^ content of the markdown file
  -> IO (Either PandocError Text) -- ^ error or the final text
renderDisplay docId = renderWith writeHtml5String (cosmeticFilters <> functionalFilters docId)

renderWith
  :: (WriterOptions -> Pandoc -> PandocIO a) -- ^ writer
  -> [PandocFilterM IO]                 -- ^ a list of pandoc filters
  -> HTMLTemplate                            -- ^ template
  -> Text                                    -- ^ content of the markdown
  -> IO (Either PandocError a)               -- ^ error or the final HTML
renderWith writer fs (HTMLTemplate fp tmpl) md = runIO $ do
  pandoc'@(Pandoc meta _) <- readMarkdown defKarasuReaderOptions md

  -- denote if we should enable number sections
  let numSec = case lookupMeta "number-sections" meta of
       Just (MetaBool b) -> b
       Just _            -> False
       Nothing           -> False

  -- apply filters
  pandoc <- liftIO $ applyFiltersM fs pandoc'

  -- compile template
  res <- liftIO $ compileTemplate fp tmpl

  case res of
    Left e -> throwError $ PandocTemplateError (T.pack e)
    Right template -> do
      let wOpts = (defKarasuWriterOptions numSec) { writerTemplate = Just template }
      writer wOpts pandoc
