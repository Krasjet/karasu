-- | Pandoc Filters for Karasu
module Karasu.Pandoc.Filters (PandocFilterIO, cosmeticFilters, functionalFilters, applyPandocFiltersIO) where

import Karasu.Pandoc.Filters.DashFilter
import Karasu.Pandoc.Filters.KernFilter
import Karasu.Pandoc.Filters.SlashFilter
import Karasu.Pandoc.Filters.SmcpFilter
import Karasu.Pandoc.Filters.LaTeXFilter
import Karasu.Pandoc.Filters.Utils
import Karasu.Models

-- | These filters are only applied during final rendering and are disabled
-- during preview
cosmeticFilters :: [PandocFilterIO]
cosmeticFilters =
  [ dashFilter
  , kernFilter
  , slashFilter
  ]

-- | These filters are always applied
functionalFilters :: DocId -> [PandocFilterIO]
functionalFilters docId =
  [ latexFilterBlock docId
  , latexFilterInline docId
  , smcpFilter
  ]
