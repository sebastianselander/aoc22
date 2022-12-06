module Day6.Day6
  ( solve1
  , solve2
  ) where

import Misc

check :: Int -> String -> Int
check amount xs = bool
        (1 + check amount (tail xs))
        amount
        ((== amount) . length . nub . take amount $ xs)

solve1 :: String -> String
solve1 = show . check 4

solve2 :: String -> String
solve2 = show . check 14
