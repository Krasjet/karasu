{-# LANGUAGE QuasiQuotes #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | For post-processing SVG files
module Karasu.Pandoc.Filters.LaTeX.PostProcessors (getBaseline, applyBaselineCorrection) where

import Karasu.Pandoc.Filters.LaTeX.Definitions
import Karasu.Pandoc.Filters.LaTeX.Utils

import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P

import Control.Applicative (some, (<|>))
import Data.List           (foldl')
import PyF

-- | We will retreive the baseline from viewbox parameters
viewboxMarker :: String
viewboxMarker = " viewBox='"

-- | Get the baseline correction of the SVG, i.e. the height of the descenders
getBaseline :: SVG -> Double
getBaseline str = getBaseline' sfx
  where
    (_pfx, sfx) = spanL viewboxMarker str

-- | A helper function for actually retrieving the baseline
getBaseline' :: String -> Double
getBaseline' sfx = case P.parse parser "<input>" sfx of
  Left err -> error $ show (err, sfx)
  Right x  -> negate x
  where
    parser :: P.Parser Double
    parser = do
      _ <- P.string viewboxMarker
      _ <- lexeme double
      _ <- P.spaces
      miny <- lexeme double -- ^ min-y
      _ <- lexeme double
      h <- lexeme double    -- ^ height
      return (miny + h)

    double :: P.Parser Double
    double = sign <*> (float1 <|> float2)

    float1 :: P.Parser Double
    float1 = do
      d <- decimal
      f <- P.option 0 (P.char '.' *> fraction)
      return (d + f)

    float2 :: P.Parser Double
    float2 = P.char '.' *> fraction

    decimal :: P.Parser Double
    decimal = foldl' (\x d -> 10 * x + digitToInt d) 0
      <$> digits1

    fraction :: P.Parser Double
    fraction = uncurry (/) . foldl' (\(x,n) d -> (10*x + digitToInt d,n*10)) (0,1)
      <$> digits1

    digits1 = some P.digit

    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt '2' = 2
    digitToInt '3' = 3
    digitToInt '4' = 4
    digitToInt '5' = 5
    digitToInt '6' = 6
    digitToInt '7' = 7
    digitToInt '8' = 8
    digitToInt '9' = 9
    digitToInt _   = 0

    sign :: P.Parser (Double -> Double)
    sign = P.option id (negate <$ P.char '-')

    lexeme :: P.Parser a -> P.Parser a
    lexeme p = p <* P.spaces

-- | Alter the SVG image to include baseline correction
applyBaselineCorrection :: SVG -> SVG
applyBaselineCorrection xml =
  [fmt|{pfx} style='vertical-align: {baseline:.6}pt' {sfx'}|]
    where
      (_, svg)   = spanL "<svg" xml
      (pfx, sfx) = spanL viewboxMarker svg
      baseline   = getBaseline' sfx
      -- we will remove the annoying id as well
      (preG, gTag) = spanL "<g " sfx
      (_, postG) = spanR '>' gTag
      sfx' = preG <> "<g>" <> postG

