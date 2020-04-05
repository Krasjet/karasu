{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter to transform capital letters into small caps
module Karasu.Pandoc.Filters.SmcpFilter (smcpFilter) where

import Karasu.Pandoc.Filters.Utils

import qualified Data.Text as T

import Data.Char   (isUpper)
import Data.Map   (update)
import Data.Text   (Text)
import Text.Pandoc

-- | Check if a character should be smcp (more precisely the '.cap' class)
isSmcp :: Char -> Bool
isSmcp c = isUpper c || c == '#'

-- | Convert the string to a span if necessary
capStr :: Text -> Inline
capStr "" = Str ""
capStr str
  | isSmcp $ T.head str = Span ("", ["cap"], []) [Str str]
  | otherwise = Str str

-- | Convert capital letters to smallcaps
smcpFilter' :: Inline -> [Inline]
smcpFilter' (Str str) =
  map capStr $
  -- group the elements to save some space
  T.groupBy bothCap str
    where
      bothCap x y = isSmcp x == isSmcp y
-- retain everything else
smcpFilter' x = [x]

-- | Only the value associated with the keys in this list will be small capped
-- in Meta
metaWhitelist :: [Text]
metaWhitelist = ["title", "pagetitle"]

smcpFilter :: PandocFilterIO Pandoc
smcpFilter (Pandoc (Meta meta) blocks) = do
  let blocks' = toPandocFilter smcpFilter' blocks
      meta' = foldr (update $ Just . toPandocFilter smcpFilter') meta metaWhitelist
  return $ Pandoc (Meta meta') blocks'
