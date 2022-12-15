module Day1.Day1
  ( solve1
  , solve2
  ) where

import Misc

parseInput :: String -> [[Int]]
parseInput = map (map read) . splitOn [""] . lines

solve1 :: String -> IO ()
solve1 = print . maximum . map sum . parseInput

solve2 :: String -> IO ()
solve2 = print . sum . take 3 . reverse . sort . map sum . parseInput
