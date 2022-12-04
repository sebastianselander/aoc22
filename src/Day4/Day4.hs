module Day4.Day4
  ( solve1
  , solve2
  ) where

import Misc

solve1 :: String -> String
solve1 = show . sum . check contained1 . parse

check  :: ((Int, Int) -> (Int, Int) -> Int) -> [(Int, Int)] -> [Int]
check f (x:y:zs) = f x y : check f zs
check _ _        = []

parse :: String -> [(Int, Int)]
parse = concatMap (map parseH . splitOn ",") . lines
    where
      parseH :: String -> (Int,Int)
      parseH str = (read $ takeWhile (/='-') str, read $ drop 1 $ dropWhile (/='-') str)

solve2 :: String -> String
solve2 = show . sum . check contained2 . parse

contained1 :: (Int, Int) -> (Int, Int) -> Int
contained1 (a,b) (c,d)
  | (a <= c && b >= d) || (c <= a && d >= b) = 1
  |  otherwise                               = 0

contained2 :: (Int, Int) -> (Int, Int) -> Int
contained2 (a,b) (c,d)
  | a <= c && b >= c || a >= c && a <= d = 1
  |  otherwise                             = 0
