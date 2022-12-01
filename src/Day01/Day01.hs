module Day01.Day01
  ( solve1
  , solve2
  ) where

import Data.List (groupBy, sort)
import Data.Maybe (maybe)
import Misc

path :: String
path = "/Day01/input.txt"

parseInput :: String -> IO [[Int]]
parseInput file = map (map read) . splitOn [""] . lines <$> getInput file

solve1 :: IO Int
solve1 = head . reverse . sort . map sum <$> parseInput path

solve2 :: IO Int
solve2 = sum . take 3 . reverse . sort . map sum <$> parseInput path
