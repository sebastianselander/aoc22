module Day4.Day4
  ( solve1
  , solve2
  ) where

import Misc
import Data.Bifunctor (bimap, second)

parse :: String -> [(Int, Int)]
parse = concatMap (map (bimap read read . second (drop 1) . span (/='-')) . splitOn ",") . lines

check  :: ((Int, Int) -> (Int, Int) -> Bool) -> [(Int, Int)] -> [Bool]
check f (x:y:zs) = f x y : check f zs
check _ _        = []

solve1 :: String -> String
solve1 = show . length . filter id . check fullyContained . parse

solve2 :: String -> String
solve2 = show . length . filter id . check overlaps . parse
