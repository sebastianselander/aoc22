module Day25.Day25
  ( solve1
  , solve2
  ) where

import Misc

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

input = unsafePerformIO $ readFile "src/Day25/input.txt"

data Snafu = Zero | One | Two | Minus | DoubleMinus
  deriving Show

parse :: String -> [[Snafu]]
parse = map toSnafu . lines
  where
    toSnafu :: String -> [Snafu]
    toSnafu [] = []
    toSnafu (x:xs) =
     case x of
       '-' -> DoubleMinus : toSnafu xs
       '=' -> Minus : toSnafu xs
       '0' -> Zero : toSnafu xs
       '1' -> One : toSnafu xs
       '2' -> Two : toSnafu xs

snafuToDec :: [Snafu] -> Int
snafuToDec [] = 0
snafuToDec (x:xs) = (toNum x * 5 ^(length xs)) + snafuToDec xs
  where
      toNum :: Snafu -> Int
      toNum Zero = 0
      toNum One = 1
      toNum Two = 2
      toNum Minus = -1
      toNum DoubleMinus = -2

decToSnafu :: Int -> [Snafu]

solve1 :: String -> IO ()
solve1 = putStrLn . const "unsolved"

solve2 :: String -> IO ()
solve2 = putStrLn . const "unsolved"

sample :: String
sample = "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122\n"

