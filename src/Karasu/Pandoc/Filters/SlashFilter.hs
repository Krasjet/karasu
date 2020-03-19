{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that adds a soft word break (<wbr>) after '/'
module Karasu.Pandoc.Filters.SlashFilter (slashFilter) where

import qualified Data.Text as T

import Data.List.Split (dropBlanks, onSublist, split)
import Text.Pandoc

-- | A helper function for slashFilter, which replaces / with /<wbr> in a
-- string
appendBreak :: String -> [Inline] -> [Inline]
appendBreak "/" acc = Str "/" : RawInline (Format "html") "<wbr>" : acc
appendBreak x   acc = Str (T.pack x) : acc

-- | Add a soft word break (<wbr>) after '/'
slashFilter :: Inline -> [Inline]
slashFilter (Str str) = foldr appendBreak [] $
                        split (dropBlanks $ onSublist "/") $ T.unpack str
slashFilter x = [x]
