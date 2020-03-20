{-# LANGUAGE QuasiQuotes #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Definitions for LaTeX filter
module Karasu.Pandoc.Filters.LaTeX.Definitions (
  TeXString,
  TeXDoc,
  BaseLine,
  SVG,
  RenderError(..),
  LaTeXEnvOptions(..),
  displaymath,
  math
) where

import Karasu.Pandoc.Filters.LaTeX.Quote

import qualified Control.Exception as E
import qualified Data.Text.Lazy    as LT

-- | A raw TeX string. Can be either a formula or environment
type TeXString = String

-- | A TeX document
type TeXDoc = LT.Text

-- | Number of points (@pt@) from the bottom of the image to the typesetting
-- baseline (min-y).
type BaseLine = Double

-- | A source of 'SVG' image.
type SVG = String

-- | This type contains all possible errors than can happen while rendering an
-- equation.  It includes all IO errors that can happen as well as more
-- specific errors.
data RenderError
  = LaTeXFailure String       -- ^ @latex@ returned a nonzero error code
  | DVISVGMFailure String     -- ^ @dvisvgm@ returned a nonzero error code
  | IOException E.IOException -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
  deriving (Show, Eq)

-- | This allows us to give different environment different options
data LaTeXEnvOptions = LaTeXEnvOptions
 { environment :: String -- ^ name of the LaTeX environment
 , preamble    :: String -- ^ The premble for the environment
 }
  deriving (Eq, Show, Read, Ord)

displaymath :: LaTeXEnvOptions
displaymath = LaTeXEnvOptions [kfmt|\
\\usepackage{amsmath}
\\usepackage{amsfonts}\
|] "displaymath"

math :: LaTeXEnvOptions
math = displaymath { environment = "math" }
