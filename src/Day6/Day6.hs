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

solve1 :: String -> IO ()
solve1 = print . check 4

solve2 :: String -> IO ()
solve2 = print . check 14
