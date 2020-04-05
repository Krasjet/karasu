{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that prevents orphaned em dashes
module Karasu.Pandoc.Filters.DashFilter (dashFilter) where

import Karasu.Pandoc.Filters.Utils

import qualified Data.Text as T

import Data.List.Split (dropBlanks, onSublist, split)
import Text.Pandoc

-- | The em dash character
emDash :: String
emDash ="\8212"

-- | A helper function for dashFilter that adds nowrap to strings like "str—"
appendNoWrap :: [String] -> [Inline]
appendNoWrap (x : "\8212" : xs) =
  Span ("", ["nowrap"], []) [Str $ T.pack (x ++ emDash)] : appendNoWrap xs
appendNoWrap (x : xs) = Str (T.pack x) : appendNoWrap xs
appendNoWrap [] = []

-- | Do not break em dashes!
dashFilter' :: Inline -> [Inline]
dashFilter' (Str str) = appendNoWrap $
  split (dropBlanks $ onSublist emDash) $ T.unpack str
dashFilter' x = [x]

dashFilter :: PandocFilterIO Pandoc
dashFilter = toPandocFilterIO dashFilter'
