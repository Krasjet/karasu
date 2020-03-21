{-# LANGUAGE QuasiQuotes #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Options for LaTeX environments
module Karasu.Pandoc.Filters.LaTeX.EnvOpts (
  LaTeXEnv,
  Preamble,
  findEnv,
  envTable,
  lookupPreamble,
  mathEnv,
  tikzEnv
) where

import Karasu.Pandoc.Filters.LaTeX.Quote
import Karasu.Pandoc.Filters.LaTeX.Utils
import Karasu.Pandoc.Filters.LaTeX.Definitions

import qualified Data.Map as Map

import Data.Map   (Map)
import Data.List
import Data.Maybe (fromMaybe)

type LaTeXEnv = String
type Preamble = String

-- | the marker for begin environment statement
beginMarker :: String
beginMarker = "\\begin{"

-- | Search for the environment
findEnv :: TeXString -> Maybe LaTeXEnv
findEnv str = do
  -- find env inside begin
  let (_, beg) = spanL beginMarker str
  env <- stripPrefix beginMarker beg
  -- strip everything behind
  return $ takeWhile (/= '}') env


-- | A lookup table for environments
-- maybe a case of statement should suffice
envTable :: Map LaTeXEnv Preamble
envTable = Map.fromList
  [ ("math", mathEnv)
  , ("display", mathEnv)
  , ("tikzpicture", tikzEnv)
  ]

-- | Lookup preamble from LaTeX environment
lookupPreamble :: Maybe LaTeXEnv -> Preamble
lookupPreamble = fromMaybe mathEnv . (flip Map.lookup envTable =<<)

-- * Environments

mathEnv :: LaTeXEnv
mathEnv = [kfmt|\
\\usepackage{amssymb}
\\usepackage{amsfonts}
\\renewcommand*{\\vec}[1]{\\mathbf{#1}}
\\newcommand*{\\im}{\\ensuremath{{\\mkern0.7mu\\mathrm{i}\\mkern1mu}}}
\\newcommand*{\\E}{\\ensuremath{{\\mkern1mu\\mathrm{e}\\mkern1mu}}}
\\newcommand*{\\dif}{\\mathop{}\\!\\mathrm{d}}
\\newcommand*{\\N}{\\ensuremath{\\mathbb{N}}}
\\newcommand*{\\Z}{\\ensuremath{\\mathbb{Z}}}
\\newcommand*{\\Q}{\\ensuremath{\\mathbb{Q}}}
\\newcommand*{\\R}{\\ensuremath{\\mathbb{R}}}
\\newcommand*{\\Cplx}{\\ensuremath{\\mathbb{C}}}
\\newcommand*{\\Trans}{\\ensuremath{{\\mkern-1.5mu\\mathsf{T}}}}
\\DeclareMathOperator{\\lcm}{lcm}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
\\renewcommand*{\\star}{{\\mathbin{\\text{\\usefont{OML}{cmm}{m}{it}\\symbol{"3F}}}}}
\\renewcommand*{\\ast}{{\\mathbin{\\text{\\usefont{OMS}{cmsy}{m}{n}\\symbol{"03}}}}}
\\DeclareFontEncoding{FML}{}{}
\\DeclareFontSubstitution{FML}{futm}{m}{it}
\\renewcommand*{\\theta}{{\\mathord{\\text{\\usefont{FML}{futmi}{m}{it}\\symbol{"12}}}}}
\\DeclareFontFamily{U}{wncy}{}
\\DeclareFontShape{U}{wncy}{m}{n}{<<->>wncyr10}{}
\\newcommand*{\\Shah}{{\\mathord{\\text{\\usefont{U}{wncy}{m}{n}\\symbol{"58}}}}}
\\makeatletter
\\renewenvironment{bmatrix}{\\left[\\mkern3mu\\env@matrix}{\\endmatrix\\mkern3mu\\right]}
\\renewenvironment{vmatrix}{\\left\\lvert\\mkern5mu\\env@matrix}{\\endmatrix\\mkern5mu\\right\\rvert}
\\makeatother
|]

tikzEnv :: LaTeXEnv
tikzEnv = mathEnv <> [kfmt|\
\\usepackage{tikz}
\\usetikzlibrary{arrows,calc,patterns,angles,quotes,3d,arrows.meta,positioning}
|]
