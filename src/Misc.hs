module Misc
  (  module Data.List.Split
  , module Data.Char
  , module Data.List
  , module Data.Function
  , fullyContained
  , overlaps
  ) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Function

fullyContained :: Ord a => (a, a) -> (a, a) -> Bool
fullyContained (a,b) (c,d) = (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (a,b) (c,d) = a <= c && b >= c || a >= c && a <= d
