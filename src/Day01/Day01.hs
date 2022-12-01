module Day01.Day01
  ( solve1
  , solve2
  ) where

import Data.List (groupBy, sort)
import Misc

path :: String
path = "/Day01/input.txt"

parseInput :: String -> IO [[Int]]
parseInput file = map (map read) . splitOn [""] . lines <$> getInput file

solve1 :: IO String
solve1 = show . maximum . map sum <$> parseInput path

solve2 :: IO String
solve2 = show . sum . take 3 . reverse . sort . map sum <$> parseInput path
