-- | Pandoc Filters for Karasu
module Karasu.Pandoc.Filters (cosmeticFilters, functionalFilters) where

import Karasu.Models

import Text.Pandoc.Fltr.DashFilter
import Text.Pandoc.Fltr.KernFilter
import Text.Pandoc.Fltr.LaTeXFilter
import Text.Pandoc.Fltr.LinkFilter
import Text.Pandoc.Fltr.SlashFilter
import Text.Pandoc.Fltr.SmcpFilter

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
functionalFilters docId =
  [ toFilterM linkFilter
  , latexFilterBlock docId
  , latexFilterInline docId
  , toFilterM smcpFilter
  ]
