module Misc
  ( module Data.List.Split
  , module Data.Char
  , module Data.List
  , module Data.Function
  , module Data.Maybe
  , module Data.Bool
  , module Data.Bifunctor
  , fullyContained
  , overlaps
  , unsafePerformIO
  , trace
  , readMaybe
  , both
  , blockOf2
  , addTuples
  , visualize
  ) where

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

addTuples :: Num a => (a,a) -> (a,a) -> (a,a)
addTuples (a,b) (c,d) = (a+c,b+d)

ctrace :: Show a => a -> a
ctrace x = trace (show x) x

dupOnPred :: (a -> Bool) -> [a] -> [a]
dupOnPred p = foldr (\x acc -> if p x then x : x : acc else x : acc) []

-- visualize :: Foldable t => t (Int,Int) -> IO ()
visualize xs = do
    print sx
    print sy
    print bx
    print by
    where
      ((sx,sy),(bx,by)) = ( foldl' (\(x,y) (a,b) -> (min x a, min y b)) (maxBound, maxBound) xs,
                            foldl' (\(x,y) (a,b) -> (max x a, max y b)) (minBound, minBound) xs
                          )
      normX = 0 - sx
      normY = 0 - sy
      normalized = foldl' (\acc (a,b) -> [(a+normX,b+normY)] ++ acc) [] xs
      tupComp (a,b) (c,d) = if a `compare` c == EQ then c `compare` d else a `compare` c
