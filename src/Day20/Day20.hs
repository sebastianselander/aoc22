module Day20.Day20
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map (Map)
import Data.Map qualified as M

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

inp = unsafePerformIO $ readFile "src/Day20/input.txt"

sample = "1\n2\n-3\n3\n-2\n0\n4\n"

type Parser = P.Parsec Void String

lineP :: Parser Int
lineP = do
    num <- L.signed (return ()) L.decimal
    _ <- P.eol
    return num

fileP :: Parser [Int]
fileP = P.someTill lineP P.eof

parse :: String -> Maybe [Int]
parse = P.parseMaybe fileP

solve1 :: String -> IO ()
solve1 = putStrLn . const "unsolved"

solve2 :: String -> IO ()
solve2 = putStrLn . const "unsolved"
