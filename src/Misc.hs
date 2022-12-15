module Misc
  ( module Data.List.Split
  , module Data.Char
  , module Data.List
  , module Data.Function
  , module Data.Maybe
  , module Data.Bool
  , module Data.Bifunctor
  , module Data.Void
  , module Control.Applicative
  , fullyContained
  , overlaps
  , unsafePerformIO
  , trace
  , ctrace
  , readMaybe
  , both
  , blockOf2
  , opPairs
  , getManhattan
  ) where

import Data.Void (Void)
import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Bool
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace)
import Data.Bifunctor
import Text.Read (readMaybe)
import Data.Foldable
import Data.Vector qualified as V

fullyContained :: Ord a => (a, a) -> (a, a) -> Bool
fullyContained (a, b) (c, d) = (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (a, b) (c, d) = a <= c && b >= c || a >= c && a <= d

both :: Bifunctor bifunctor => (a -> b) -> bifunctor a a -> bifunctor b b
both f = bimap f f

blockOf2 :: [a] -> [(a,a)]
blockOf2 []       = []
blockOf2 (x:y:ys) = (x,y) : blockOf2 ys

opPairs :: Num a => (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
opPairs f (a,b) (c,d) = (f a c, f b d)

ctrace :: Show a => a -> a
ctrace x = trace (show x) x

dupOnPred :: (a -> Bool) -> [a] -> [a]
dupOnPred p = foldr (\x acc -> if p x then x : x : acc else x : acc) []

getManhattan :: (Int,Int) -> (Int,Int) -> Int
getManhattan (x,y) (xx,yy) = abs (xx - x) + abs (yy -y)

