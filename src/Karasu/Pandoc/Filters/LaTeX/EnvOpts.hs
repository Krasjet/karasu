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
\\usepackage{amsmath}
\\usepackage{amsfonts}\
|]

tikzEnv :: LaTeXEnv
tikzEnv = [kfmt|\
\\usepackage{tikz}
\\usetikzlibrary{positioning}\
|]
