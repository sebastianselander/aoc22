module Day6.Day6
  ( solve1
  , solve2
  ) where

import Misc

check :: Int -> Int -> String -> Int
check count amount xs =
    bool
        (check (count+1) amount (tail xs))
        count
        ((length $ nub $ take amount xs) == amount)

solve1 :: String -> String
solve1 = show . check 4 4

solve2 :: String -> String
solve2 = show . check 14 14
