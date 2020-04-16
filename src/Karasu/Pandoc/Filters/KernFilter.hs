{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that add custom kerning pairs using html, designed
-- specifically for Garibaldi
module Karasu.Pandoc.Filters.KernFilter (kernFilter) where

import Karasu.Pandoc.Filters.Utils

import qualified Data.Map  as Map
import qualified Data.Text as T

import Data.List       (nub)
import Data.List.Split (dropBlanks, oneOf, split)
import Data.Map        (Map)
import Data.Text       (Text)
import Text.Pandoc

-- | The kerning table
kernTable :: Map (Char, Char) Text
kernTable = Map.fromList
  [ (('(', 'j'), ".04em")
  , (('(', 'J'), ".05em")
  , (('[', 'j'), ".06em")
  , (('[', 'J'), ".07em")
  ]
kernPrefix :: String
kernPrefix = nub . map fst $ Map.keys kernTable

-- | Convert matched kerning to a span
lookupKern :: (Char, Char) -> String -> Inline
lookupKern (ch, nch) str = case Map.lookup (ch, nch) kernTable of
  Nothing   -> Str $ T.pack str
  Just kern -> Span ("", [], [("style", "letter-spacing:" <> kern)]) [Str $ T.pack str]

-- | Add letter spacing to the first letter
appendKern :: String -> [Inline] -> [Inline]
-- if curr has only one letter and next string is not empty,
-- lookup the kerning pair
appendKern curr@[ch] acc@(Str n : _)
  | T.null n = Str (T.pack curr) : acc
  | otherwise = lookupKern (ch, T.head n) curr : acc
-- the same, but for span
appendKern curr@[ch] acc@(Span _ [Str n] : _)
  | T.null n = Str (T.pack curr) : acc
  | otherwise = lookupKern (ch, T.head n) curr : acc
-- otherwise
appendKern curr acc = Str (T.pack curr) : acc

-- | Add manual kerning to some letter pairs
kernFilter' :: Inline -> [Inline]
kernFilter' (Str str) = foldr appendKern [] $
                       split (dropBlanks $ oneOf kernPrefix) $ T.unpack str
kernFilter' x = [x]

kernFilter :: PandocFilterIO Pandoc
kernFilter = toPandocFilterIO kernFilter'
