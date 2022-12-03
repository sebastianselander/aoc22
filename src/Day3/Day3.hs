module Day3.Day3
  ( solve1
  , solve2
  ) where

import System.IO.Unsafe

import Data.List
import Misc

input :: String
input = unsafePerformIO $ readFile "Day3/test.txt"

parseItem1 :: String -> (String, String)
parseItem1 item = splitAt middle item
  where
    middle = length item `div` 2

commons :: (String, String) -> String
commons (i1, i2) = nub $ intersect i1 i2

pointify :: Char -> Int
pointify c
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

solve1 :: String -> String
solve1 = show . sum . concatMap (map pointify . commons . parseItem1) . lines

solve2 :: String -> String
solve2 = show . sum . concatMap ((map pointify) . nub . common) . split . lines
  where
    split :: [String] -> [[String]]
    split [] = []
    split (a:b:c:rest) = [a, b, c] : split rest
    common :: [String] -> String
    common (a:b:c:[]) = intersect (intersect a b) c
