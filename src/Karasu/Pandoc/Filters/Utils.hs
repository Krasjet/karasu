{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Some utility functions for handling pandoc filters
module Karasu.Pandoc.Filters.Utils (PandocFilterIO(..), applyPandocFilter, applyPandocFilters) where

import Control.Monad.IO.Class (liftIO)
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Foldable (foldl')
import Control.Monad ((>=>))

-- | This class converts partial pandoc filters (i.e. at the level of Inlines
-- and Blocks) to composable (Pandoc -> IO Pandoc) filters.
class PandocFilterIO a where
  toPandocFilterIO
    :: a                   -- ^ partial filter
    -> Pandoc -> IO Pandoc -- ^ the output pandoc filter

instance (Walkable a Pandoc) => PandocFilterIO (a -> a) where
  toPandocFilterIO = (return .) . walk

instance (Walkable a Pandoc) => PandocFilterIO (a -> IO a) where
  toPandocFilterIO = walkM

instance (Walkable [a] Pandoc) => PandocFilterIO (a -> [a]) where
  toPandocFilterIO = (return .) . walk . concatMap

instance (Walkable [a] Pandoc) => PandocFilterIO (a -> IO [a]) where
  toPandocFilterIO = walkM . (fmap concat .) . mapM

-- Apply a single partial filter
applyPandocFilter
  :: (PandocFilterIO a)
  => a                         -- ^ the partial filter
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFilter = (liftIO .)  . toPandocFilterIO

-- Apply a list of partial filters from left to right
applyPandocFilters
  :: (PandocFilterIO a)
  => [a]                       -- ^ a list of partial filters
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFilters = foldl' (>=>) return . map applyPandocFilter
-- applyPandocFilters = foldr ((>=>) . applyPandocFilter) return
