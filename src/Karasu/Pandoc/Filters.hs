-- | Pandoc Filters for Karasu
module Karasu.Pandoc.Filters (cosmeticFilters, functionalFilters) where

import Karasu.Models
import Karasu.Pandoc.Filters.DashFilter
import Karasu.Pandoc.Filters.KernFilter
import Karasu.Pandoc.Filters.LaTeXFilter
import Karasu.Pandoc.Filters.LinkFilter
import Karasu.Pandoc.Filters.SlashFilter
import Karasu.Pandoc.Filters.SmcpFilter

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
