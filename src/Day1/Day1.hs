module Day1
  ( solve1
  , solve2
  ) where

import Data.List (groupBy, sort)
import Data.Maybe (maybe)

input :: IO String
input = readFile "input.txt"

safeRead :: String -> Int
safeRead "" = 0
safeRead x = read x

solve1 :: IO Int
solve1 = do
  inp <- map safeRead . lines <$> input
  pure . head . reverse . sort . map sum . groupBy (\a b -> a > 0 && b > 0) $
    inp

solve2 :: IO Int
solve2 = do
  inp <- map safeRead . lines <$> input
  let (a:b:c:_) =
        reverse . sort . map sum . groupBy (\a b -> a > 0 && b > 0) $ inp
  pure $ a + b + c
