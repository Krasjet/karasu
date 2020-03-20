{-# LANGUAGE OverloadedStrings #-}

-- | A filter to transform TeX strings to svg images
module Karasu.Pandoc.Filters.LaTeXFilter (latexFilterInline) where

import Karasu.Models
import Karasu.Pandoc.Filters.LaTeX.Definitions
import Karasu.Pandoc.Filters.LaTeX.PostProcessors
import Karasu.Pandoc.Filters.LaTeX.Renderer
import Karasu.Pandoc.Filters.Utils

import qualified Data.Text as T

import System.FilePath        ((</>))
import Text.Pandoc.Definition

-- | Render errors nicely, in order to show any problems clearly, with all
-- information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure str) = pandocError [Str "LaTeX failed:", LineBreak, Code nullAttr $ T.pack str]
displayError (DVISVGMFailure str) = pandocError [Str "dvisvgm failed:", LineBreak, Code nullAttr $ T.pack str]
displayError (IOException e) = pandocError [Str "IO Exception:", LineBreak, Code nullAttr $ T.pack $ show e]

pandocError :: [Inline] -> Inline
pandocError = Strong . (Emph [Str "Error:"] :)

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: DocId
  -> Inline
  -> IO Inline
latexFilterInline' docId (Math mathType texStr) = do
  let texDoc = mkMathTeXDoc mathType $ T.unpack texStr
      cacheDir = "cache" </> docId
  res <- compileSVG cacheDir texDoc
  case res of
    Left e    -> return $ displayError e
    Right svg -> return $ RawInline (Format "html") $ T.pack $ applyBaselineCorrection svg
latexFilterInline' _ x = return x

latexFilterInline :: DocId -> PandocFilterIO
latexFilterInline = toPandocFilterIO . latexFilterInline'
