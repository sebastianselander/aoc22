module Day4.Day4
  ( solve1
  , solve2
  ) where

import Misc
import Data.Bifunctor (bimap, second)

parse :: String -> [(Int, Int)]
parse = concatMap (map (bimap read read . second (drop 1) . span (/='-')) . splitOn ",") . lines

contained1 :: (Int, Int) -> (Int, Int) -> Int
contained1 (a,b) (c,d)
  | (a <= c && b >= d) || (c <= a && d >= b) = 1
  | otherwise                                = 0

contained2 :: (Int, Int) -> (Int, Int) -> Int
contained2 (a,b) (c,d)
  | a <= c && b >= c || a >= c && a <= d = 1
  | otherwise                            = 0

check  :: ((Int, Int) -> (Int, Int) -> Int) -> [(Int, Int)] -> [Int]
check f (x:y:zs) = f x y : check f zs
check _ _        = []

solve1 :: String -> String
solve1 = show . sum . check contained1 . parse

solve2 :: String -> String
solve2 = show . sum . check contained2 . parse
