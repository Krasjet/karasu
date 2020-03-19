{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}

-- | Some utility functions for handling pandoc filters
module Karasu.Pandoc.Filters.Utils (PandocFilterIO, toPandocFilterIO, applyPandocFilterIO, applyPandocFiltersIO) where

import Control.Monad          ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable          (foldl')
import Text.Pandoc
import Text.Pandoc.Walk

-- | A synonym for pandoc filter
type PandocFilterIO = Pandoc -> IO Pandoc

-- | This class converts partial pandoc filters (i.e. at the level of Inlines
-- and Blocks) to composable (Pandoc -> IO Pandoc) filters.
class ToPandocFilterIO a where
  toPandocFilterIO
    :: a              -- ^ partial filter
    -> PandocFilterIO -- ^ the output pandoc filter

instance (Walkable a Pandoc) => ToPandocFilterIO (a -> a) where
  toPandocFilterIO = (return .) . walk

instance (Walkable a Pandoc) => ToPandocFilterIO (a -> IO a) where
  toPandocFilterIO = walkM

instance (Walkable [a] Pandoc) => ToPandocFilterIO (a -> [a]) where
  toPandocFilterIO = (return .) . walk . concatMap

instance (Walkable [a] Pandoc) => ToPandocFilterIO (a -> IO [a]) where
  toPandocFilterIO = walkM . (fmap concat .) . mapM

-- Apply a single pandoc filter
applyPandocFilterIO
  :: PandocFilterIO            -- ^ a single pandoc filter
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFilterIO = (liftIO .)

-- Apply a list of pandoc filters from left to right
applyPandocFiltersIO
  :: [PandocFilterIO]          -- ^ a list of pandoc filters
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFiltersIO = foldl' (>=>) return . map applyPandocFilterIO
