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

solve1 :: String -> IO ()
solve1 = print . length . filter id . check fullyContained . parse

solve2 :: String -> IO ()
solve2 = print . length . filter id . check overlaps . parse
