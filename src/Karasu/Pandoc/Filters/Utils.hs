{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Some utility functions for handling pandoc filters
module Karasu.Pandoc.Filters.Utils
  ( PandocFilter
  , toPandocFilter
  , PandocFilterIO
  , toPandocFilterIO
  , applyPandocFilterIO
  , applyPandocFiltersIO
  ) where

import Control.Monad          ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable          (foldl')
import Text.Pandoc
import Text.Pandoc.Walk

-- | A synonym for pandoc filter, @p@ refers to the *level* of the filter. It's usually Pandoc. We don't actually need this type, because it is an instance of ToPandocFilterIO and we can easily convert it to a PandocFilterIO
type PandocFilter p = p -> p

-- | A pandoc filter with IO monad
type PandocFilterIO p = p -> IO p

-- | This class converts partial pandoc filters (i.e. at the level of @a@, usually being Inlines and Blocks) to composable (@p@ -> @p@) filters, where @p@ is usually Pandoc.
class ToPandocFilter f p where
  toPandocFilter
    :: f                -- ^ partial filter
    -> PandocFilter p   -- ^ the output pandoc filter

instance (Walkable a p) => ToPandocFilter (a -> a) p where
  toPandocFilter = walk

instance (Walkable [a] p) => ToPandocFilter (a -> [a]) p where
  toPandocFilter = walk . concatMap

-- | This class converts partial pandoc filters (i.e. at the level of @a@, usually being Inlines and Blocks) to composable (@p@ -> IO @p@) filters, where @p@ is usually Pandoc.
class ToPandocFilterIO f p where
  toPandocFilterIO
    :: f                -- ^ partial filter
    -> PandocFilterIO p -- ^ the output pandoc IO filter

instance (Walkable a p) => ToPandocFilterIO (a -> a) p where
  toPandocFilterIO = (return .) . toPandocFilter

instance (Walkable a p) => ToPandocFilterIO (a -> IO a) p where
  toPandocFilterIO = walkM

instance (Walkable [a] p) => ToPandocFilterIO (a -> [a]) p where
  toPandocFilterIO = (return .) . toPandocFilter

instance (Walkable [a] p) => ToPandocFilterIO (a -> IO [a]) p where
  toPandocFilterIO = walkM . (fmap concat .) . mapM

-- Apply a single pandoc filter
applyPandocFilterIO
  :: PandocFilterIO Pandoc     -- ^ a single pandoc filter
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFilterIO = (liftIO .)

-- Apply a list of pandoc filters from left to right
applyPandocFiltersIO
  :: [PandocFilterIO Pandoc]   -- ^ a list of pandoc filters
  -> Pandoc -> PandocIO Pandoc -- ^ to be used in PandocIO monad
applyPandocFiltersIO = foldl' (>=>) return . map applyPandocFilterIO
