module Day1.Day1
  ( solve1
  , solve2
  ) where

import Misc

parseInput :: String -> [[Int]]
parseInput = map (map read) . splitOn [""] . lines

solve1 :: String -> String
solve1 = show . maximum . map sum . parseInput

solve2 :: String -> String
solve2 = show . sum . take 3 . reverse . sort . map sum . parseInput
