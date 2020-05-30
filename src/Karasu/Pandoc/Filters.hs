{-# LANGUAGE OverloadedStrings #-}

-- | Pandoc Filters for Karasu
module Karasu.Pandoc.Filters (cosmeticFilters, functionalFilters) where

import Karasu.Models

import Text.Pandoc.Fltr.BreakCodeFilter
import Text.Pandoc.Fltr.DashFilter
import Text.Pandoc.Fltr.KernFilter
import Text.Pandoc.Fltr.LaTeXFilter
import Text.Pandoc.Fltr.LinkFilter
import Text.Pandoc.Fltr.ImageFilter
import Text.Pandoc.Fltr.SlashFilter
import Text.Pandoc.Fltr.SmcpFilter
import Text.Pandoc.Fltr.ParaFilter

import Text.Pandoc.Filter.Utils

-- | These filters are only applied during final rendering and are disabled
-- during preview
cosmeticFilters :: [PandocFilterM IO]
cosmeticFilters = map toFilterM
  [ dashFilter
  , kernFilter
  , slashFilter
  ]

-- | These filters are always applied
functionalFilters :: DocId -> [PandocFilterM IO]
functionalFilters dId =
  [ toFilterM paraFilter
  , toFilterM imageFilter
  , toFilterM linkFilter
  , toFilterM $ breakCodeFilter 8
  , latexFilter $ defOpts { docId = Just dId }
  , toFilterM smcpFilter
  ]

defOpts :: LaTeXFilterOptions
defOpts = def
  { cacheDir = Just "cache"
  , tempDir = Just ".tmp"
  }
