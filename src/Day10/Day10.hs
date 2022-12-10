module Day10.Day10
  ( solve1
  , solve2
  ) where

import Misc

data Instruction = Noop | Cycle | Addx Int
    deriving Show

parse :: String -> [Instruction]
parse = reverse . foldl' (\acc x -> readInstruction x ++ acc) [] . lines
    where
      readInstruction :: String -> [Instruction]
      readInstruction = \case
        "noop" -> [Noop]
        addx   -> [Addx (read $ drop 5 addx) , Cycle]

execute :: Int -> [Instruction] -> [Int]
execute x ins = scanl' runInstr x ins
    where
      runInstr :: Int -> Instruction -> Int
      runInstr x = \case
        Noop -> x
        Cycle -> x
        Addx v -> x + v

sprite :: Int -> (Int,Int,Int)
sprite i = (i-1,i,i+1)

toPixel :: (Int,(Int,Int,Int)) -> Char
toPixel (a,(b,c,d)) = if a `elem` ([b,c,d] :: [Int])
                        then '#'
                        else '.'

solve1 :: String -> String
solve1 str = show $ sum ([ 20 * ran !! 19
                         , 60 * ran !! 59
                         , 100 * ran !! 99
                         , 140 * ran !! 139
                         , 180 * ran !! 179
                         , 220 * ran !! 219] :: [Int])
  where
    ran :: [Int]
    ran = execute 1 . parse $ str

solve2 :: String -> String
solve2 = show . chunksOf 40 . map toPixel . zip (cycle [0..39]) . map sprite . execute 1. parse
