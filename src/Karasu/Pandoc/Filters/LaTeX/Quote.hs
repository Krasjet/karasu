-- | A QuasiQuoter For cleaner formatting
module Karasu.Pandoc.Filters.LaTeX.Quote (kfmt) where

import Language.Haskell.TH.Quote

import PyF

kfmt :: QuasiQuoter
kfmt = fmtWithDelimiters ('<','>')
