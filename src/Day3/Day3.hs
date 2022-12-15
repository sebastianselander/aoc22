module Day3.Day3
  ( solve1
  , solve2
  ) where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import Misc

solve1 :: String -> IO ()
solve1 =
  print .
  sum .
  concatMap
    (map (\c -> bool (ord c - 96) (ord c - 38) (isUpper c)) .
     nub . uncurry intersect . uncurry splitAt . (flip div 2 . length &&& id)) .
  lines

solve2 :: String -> IO ()
solve2 =
  print .
  sum .
  concatMap
    (map (\c -> bool (ord c - 96) (ord c - 38) (isUpper c)) . nub . common) .
  split . lines
  where
    split :: [String] -> [[String]]
    split [] = []
    split (a:b:c:rest) = [a, b, c] : split rest
    common :: [String] -> String
    common (a:b:c:_) = intersect (intersect a b) c

think :: String
think = "hej"
