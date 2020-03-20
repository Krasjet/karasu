{-# LANGUAGE OverloadedStrings #-}

-- | A filter to transform TeX strings to svg images
module Karasu.Pandoc.Filters.LaTeXFilter (latexFilterInline, latexFilterBlock) where

import Karasu.Models
import Karasu.Pandoc.Filters.LaTeX.Definitions
import Karasu.Pandoc.Filters.LaTeX.EnvOpts
import Karasu.Pandoc.Filters.LaTeX.PostProcessors
import Karasu.Pandoc.Filters.LaTeX.Renderer
import Karasu.Pandoc.Filters.Utils

import qualified Data.Text as T

import System.FilePath        ((</>))
import Text.Pandoc.Definition

-- | Render errors nicely, in order to show any problems clearly, with all
-- information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure str) =
  RawInline (Format "html") $ T.pack e
    where
      e = "<pre class=\"err\">LaTeX failed.\n" <> str <> "</pre>"
displayError (DVISVGMFailure str) =
  RawInline (Format "html") $ T.pack e
    where
      e = "<pre class=\"err\">dvisvgm failed.\n" <> str <> "</pre>"
displayError (IOException ex) =
  RawInline (Format "html") $ T.pack e
    where
      e = "<pre class=\"err\">IO exception.\n" <> show ex <> "</pre>"

-- | Render svg or error
renderError :: Either RenderError SVG -> Inline
renderError (Left e) = displayError e
renderError (Right svg) = RawInline (Format "html") $ T.pack $ postProcessSVG svg

-- | Render a TeXStr to SVG
renderTeXStr
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO (Either RenderError SVG)
renderTeXStr docId math texStr = do
  let cacheDir = "cache" </> docId
  let texDoc = case math of
       Just mt -> mkMathTeXDoc mt texStr
       Nothing -> mkTeXDoc (lookupPreamble $ findEnv texStr) texStr
  compileSVG cacheDir texDoc

-- | Render a TeXStr to inline SVG
renderTeXStrInline
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO Inline
renderTeXStrInline docId mt texStr = renderError <$> renderTeXStr docId mt texStr

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: DocId
  -> Inline
  -> IO Inline
-- math environment
latexFilterInline' docId (Math mathType texStr) =
  renderTeXStrInline docId (Just mathType) $ T.unpack texStr
-- math environment
latexFilterInline' docId (RawInline (Format "tex") texStr) =
  renderTeXStrInline docId Nothing $ T.unpack texStr
latexFilterInline' _ x = return x

-- | Render a TeXStr to block SVG
renderTeXStrBlock
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO Block
renderTeXStrBlock docId mt texStr = do
  i <- renderTeXStrInline docId mt texStr
  return $ Plain [i]

-- | Convert block TeX strings to SVG images
latexFilterBlock'
  :: DocId
  -> Block
  -> IO Block
latexFilterBlock' docId (RawBlock (Format "tex") texStr) =
  renderTeXStrBlock docId Nothing $ T.unpack texStr
latexFilterBlock' _ x = return x

latexFilterInline :: DocId -> PandocFilterIO
latexFilterInline = toPandocFilterIO . latexFilterInline'

latexFilterBlock :: DocId -> PandocFilterIO
latexFilterBlock = toPandocFilterIO . latexFilterBlock'
