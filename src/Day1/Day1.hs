module Day1
  ( solve1
  , solve2
  ) where

import Data.List (groupBy, sort)
import Data.Maybe (maybe)

-- input :: IO [[Int]]
input = readFile "Day1/input.txt"

isJusts :: Maybe a -> Maybe a -> Bool
isJusts (Just a) (Just b) = True
isJusts _ _ = False

safeRead :: String -> Maybe Int
safeRead "" = Nothing
safeRead x = pure $ read x

solve1 :: IO String -> IO Int
solve1 inp' = do
  inp <- map safeRead . lines <$> inp'
  pure $
    head . reverse . sort . map sum . (map (map (maybe 0 id))) . groupBy isJusts $
    inp

solve2 :: IO String -> IO Int
solve2 inp' = do
  inp <- map safeRead . lines <$> inp
  let (a:b:c:xs') =
        reverse . sort . map sum . (map (map (maybe 0 id))) . groupBy isJusts $
        inp
  pure $ a + b + c
