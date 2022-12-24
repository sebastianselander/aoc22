module Day16.Day16
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map (Map)
import Data.Map qualified as M

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

inp = unsafePerformIO $ readFile "src/Day16/input.txt"

sample :: String
sample = unlines
    [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
    , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
    , "Valve HH has flow rate=22; tunnel leads to valve GG"
    , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
    , "Valve JJ has flow rate=21; tunnel leads to valve II"
    ]

type Parser = P.Parsec Void String

data Valve = Valve
    { name :: String
    , flowrate :: Int
    , neighbors :: [(Int,String)]
    }
    deriving Show

stringP :: Parser String
stringP = P.someTill P.letterChar (P.char ' ')

letterOrSpace :: Parser Char
letterOrSpace = P.char ' ' <|> P.letterChar

neighP :: Parser String
neighP = P.count 2 P.upperChar

lineP :: Parser Valve
lineP = do
    _ <- P.string "Valve"
    name <- (P.char ' ' *> neighP <* P.char ' ')
    _ <- P.someTill letterOrSpace (P.char '=')
    flow <- L.decimal
    _ <- P.char ';'
    _ <- P.some (P.char ' ')
    _ <- P.count 4 stringP
    neighs <- neighP `P.sepBy` P.string ", "
    _ <-P.eol
    return $ Valve name flow (zip (repeat 1) neighs)

inputP :: Parser [Valve]
inputP = P.someTill lineP P.eof

parse :: String -> Map String Valve
parse = M.fromList
      . map (\x -> (name x, x))
      . fromMaybe (error "Parsing of input failed")
      . P.parseMaybe inputP

solve1 :: String -> IO ()
solve1 = putStrLn . const "unsolved"

solve2 :: String -> IO ()
solve2 = putStrLn . const "unsolved"

