{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | SVG Renderer for TeX strings inside a markdown file
module Karasu.Pandoc.Filters.LaTeX.Renderer (mkTeXDoc, mkMathTeXDoc, compileSVG) where

import Karasu.Pandoc.Filters.LaTeX.Definitions
import Karasu.Pandoc.Filters.LaTeX.EnvOpts
import Karasu.Pandoc.Filters.LaTeX.Quote
import Karasu.Pandoc.Filters.LaTeX.Utils

import qualified Control.Exception as E
import qualified Data.Text.Lazy    as LT

import Control.DeepSeq            (NFData (..))
import Control.Monad              (when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE,
                                   withExceptT)
import System.Exit                (ExitCode (..))
import System.FilePath            ((<.>), (</>))
import Text.Pandoc.Definition     (MathType (..))

-- | Make latex document for options
mkTeXDoc
  :: Preamble -- ^ environment and preamble
  -> TeXString       -- ^ tex string to be rendered
  -> TeXDoc          -- ^ output TeX document
mkTeXDoc preamble texString = LT.pack [kfmt|\
\\nonstopmode
\\documentclass[12pt]{article}
\\usepackage[active,tightpage]{preview}
\\usepackage{amsmath}
\\usepackage{xcolor}
\\renewcommand{\\rmdefault}{zpltlf}
\\usepackage{newpxmath}
\\usepackage[scr=rsfso, cal=pxtx, bb=ams, frak=pxtx]{mathalfa}
<preamble>
\\begin{document}
\\begin{preview}
<texString>\
\\end{preview}
\\end{document}
|]

-- | Make math latex document for options
mkMathTeXDoc
  :: MathType        -- ^ environment and preamble
  -> TeXString       -- ^ tex string to be rendered
  -> TeXDoc          -- ^ output TeX document
mkMathTeXDoc InlineMath texStr = mkTeXDoc mathEnv [kfmt|\
\\begin{math}
<texStr>
\\end{math}
|]
mkMathTeXDoc DisplayMath texStr = mkTeXDoc mathEnv [kfmt|\
\\begin{displaymath}
<texStr>
\\end{displaymath}
|]

-- | The temp directory for compiling tex file
tmpDir :: String
tmpDir = ".tmp"

-- | The basename of the temporary file for compiling tex file
tmpFile :: String
tmpFile = "compiling"

-- * More utility functions specific to the renderer

io :: NFData a => IO a -> ExceptT RenderError IO a
io = withExceptT IOException . tryIO

handler :: ExceptT e IO a -> E.IOException -> IO (Either e a)
handler rgt _ = runExceptT rgt

orElse :: IO a -> ExceptT e IO a -> ExceptT e IO a
orElse lft rgt = ExceptT $ fmap Right lft `E.catch` handler rgt

cached
  :: String                        -- ^ cache directory
  -> TeXDoc                        -- ^ tex document
  -> ExceptT RenderError IO String -- ^ action
  -> ExceptT RenderError IO String
cached cacheDir texDoc action = do
  -- the cache
  let path = cacheDir </> hashText texDoc <.> "svg"
  -- only compile a new one if no cache exists
  readFile path `orElse` do
    result <- action
    io $ writeFileHandleMissingS path result
    return result

-- | Convert a tex string into a SVG image.
compileSVG
  :: FilePath          -- cache directory for svg files
  -> TeXDoc        -- the tex string to be compiled
  -> IO (Either RenderError SVG)
compileSVG cacheDir texDoc = runExceptT $
  cached cacheDir texDoc $ do
    -- write to tex file
    io $ writeFileHandleMissing (tmpDir </> tmpFile <.> "tex") texDoc

    -- compile latex file
    -- NOTE remember to restrict file access of tex files in texmf.cnf
    -- see https://tex.stackexchange.com/questions/10418
    (exitCode, out, err) <- io $ readProcessWithCWD tmpDir "latex" [tmpFile <.> "tex"]
    when (exitCode /= ExitSuccess) $ throwE $ LaTeXFailure (out ++ "\n" ++ err)

    -- convert to svg file
    (exitCode', out', err') <- io $ readProcessWithCWD tmpDir "dvisvgm" $
      ["-n", "-j", "-Z", "1.28"] ++ ["-o", tmpFile <.> "svg", tmpFile <.> "dvi"]
      -- ^ no font and clipjoin w/ zoom 23/18
    when (exitCode' /= ExitSuccess) $ throwE $ DVISVGMFailure (out' ++ "\n" ++ err')

    -- return
    io $ readFile (tmpDir </> tmpFile <.> "svg")
