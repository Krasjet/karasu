{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that forces every link to open in a new tab
module Karasu.Pandoc.Filters.LinkFilter (linkFilter) where

import Karasu.Pandoc.Filters.Utils

import Text.Pandoc

-- | set target to _blank
linkFilter' :: Inline -> Inline
linkFilter' (Link (identi, cls, attrs) child target) =
  Link (identi, cls, ("target", "_blank") : attrs) child target
linkFilter' x = x

linkFilter :: PandocFilterIO
linkFilter = toPandocFilterIO linkFilter'
