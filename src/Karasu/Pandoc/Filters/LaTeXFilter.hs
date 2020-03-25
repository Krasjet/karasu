{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

-- | A filter to transform TeX strings to svg images
module Karasu.Pandoc.Filters.LaTeXFilter (latexFilterInline, latexFilterBlock) where

import Karasu.Models
import Karasu.Pandoc.Filters.LaTeX.Definitions
import Karasu.Pandoc.Filters.LaTeX.EnvOpts
import Karasu.Pandoc.Filters.LaTeX.PostProcessors
import Karasu.Pandoc.Filters.LaTeX.Renderer
import Karasu.Pandoc.Filters.Utils
import PyF

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Data.ByteString.Base64 as Base64
import System.FilePath        ((</>))
import Text.Pandoc.Definition

-- * Utils

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

-- | Render a TeXStr to SVG
renderTeXStr
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO (Maybe LaTeXEnv, Either RenderError SVG)
renderTeXStr docId math texStr = do
  let cacheDir = "cache" </> docId
  let (env, texDoc) = case math of
       Just InlineMath -> (Just "math", mkMathTeXDoc InlineMath texStr)
       Just DisplayMath -> (Just "displaymath", mkMathTeXDoc DisplayMath texStr)
       Nothing -> (e, mkTeXDoc (lookupPreamble e) texStr)
         where
           e = findEnv texStr
  (env,) <$> compileSVG cacheDir texDoc

-- | Render the SVG as an actual inline image element with error handling. Note
-- that we are not displaying the plain SVG inline because there are some
-- serious rendering issues in many browsers
renderHandleError :: Maybe LaTeXEnv -> Either RenderError SVG -> Inline
renderHandleError _ (Left err) = displayError err
renderHandleError env (Right svg) =
  case env of
    Nothing -> Image ("", ["tex", "noenv"], attrs) [] (svgE, "")
    --                    ^ class
    Just e  -> Image ("", ["tex", T.pack $ map escapeStar e], attrs) [] (svgE, "")
    where
      -- | escape starred environments, align* -> align_
      escapeStar :: Char -> Char
      escapeStar '*' = '_'
      escapeStar  x  =  x

      -- | post processed svg
      psvg :: SVG
      psvg = postProcessSVG svg

      -- | baseline correction of the image
      baseline :: Double
      baseline = getBaseline psvg

      -- | encoded svg image
      svgE :: T.Text
      svgE = "data:image/svg+xml;base64," `T.append` T.decodeUtf8 (Base64.encode $ BS8.pack psvg)

      -- | attributes
      attrs :: [(T.Text, T.Text)]
      attrs = [("style", [fmt|vertical-align: {baseline:.6}pt|])]

-- * Inline filter

-- | Render a TeXStr to inline SVG
renderInlineSVG
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO Inline
renderInlineSVG docId mt texStr = uncurry renderHandleError <$> renderTeXStr docId mt texStr

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: DocId
  -> Inline
  -> IO Inline
-- math environment
latexFilterInline' docId (Math mathType texStr) =
  renderInlineSVG docId (Just mathType) $ T.unpack texStr
-- tex environment
latexFilterInline' docId (RawInline (Format "tex") texStr) =
  renderInlineSVG docId Nothing $ T.unpack texStr
latexFilterInline' _ x = return x

-- * Block filter

-- | Render a TeXStr to block SVG
renderBlockSVG
  :: DocId          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO Block
renderBlockSVG docId mt texStr = do
  i <- renderInlineSVG docId mt texStr
  return $ Plain [i]

-- | Convert block TeX strings to SVG images
latexFilterBlock'
  :: DocId
  -> Block
  -> IO Block
latexFilterBlock' docId (RawBlock (Format "tex") texStr) =
  renderBlockSVG docId Nothing $ T.unpack texStr
latexFilterBlock' _ x = return x

-- * partial -> complete filter

latexFilterInline :: DocId -> PandocFilterIO
latexFilterInline = toPandocFilterIO . latexFilterInline'

latexFilterBlock :: DocId -> PandocFilterIO
latexFilterBlock = toPandocFilterIO . latexFilterBlock'
