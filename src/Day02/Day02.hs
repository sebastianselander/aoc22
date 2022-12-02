{-# LANGUAGE LambdaCase #-}

module Day02.Day02
  ( solve1
  , solve2
  ) where

import Misc

dayNumber :: String
dayNumber = "02"

path :: String
path = "/Day" <> dayNumber <> "/input.txt"

parseInput :: String -> IO [(Oppo, Self)]
parseInput path = (map tup) . lines <$> getInput path
  where
    tup :: String -> (Oppo, Self)
    tup (o:' ':s:_) = (oppo o, self s)
    oppo :: Char -> Oppo
    oppo 'A' = A
    oppo 'B' = B
    oppo 'C' = C
    self 'X' = X
    self 'Y' = Y
    self 'Z' = Z

data Self
  = X
  | Y
  | Z
  deriving (Show)

data Oppo
  = A
  | B
  | C
  deriving (Show)

--X = 1, Y=2, Z=3 : win = 6 draw = 1
win, draw, loss :: Int
win = 6

draw = 3

loss = 0

score :: (Oppo, Self) -> Int
score =
  \case
    (A, X) -> 1 + draw
    (A, Y) -> 2 + win
    (A, Z) -> 3 + loss
    (B, X) -> 1 + loss
    (B, Y) -> 2 + draw
    (B, Z) -> 3 + win
    (C, X) -> 1 + win
    (C, Y) -> 2 + loss
    (C, Z) -> 3 + draw

lose :: (Oppo, Self) -> Int
lose (a, b) =
  case (a, b) of
    (A, X) -> loss + 3
    (A, Y) -> draw + 1
    (A, Z) -> win + 2
    (B, X) -> loss + 1
    (B, Y) -> draw + 2
    (B, Z) -> win + 3
    (C, X) -> loss + 2
    (C, Y) -> draw + 3
    (C, Z) -> win + 1

solve1 :: IO String
solve1 = show . sum . map score <$> parseInput path

solve2 :: IO String
solve2 = show . sum . map lose <$> parseInput path
