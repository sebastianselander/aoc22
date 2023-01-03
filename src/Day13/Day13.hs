{-# LANGUAGE ImportQualifiedPost #-}

module Day13.Day13 (
  solve1,
  solve2,
) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Misc
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = P.Parsec Void String

data Packet = List [Packet] | Number Int
  deriving (Show, Eq)

numberP :: Parser Packet
numberP = Number <$> L.decimal

listP :: Parser Packet
listP = List <$> (P.char '[' *> ((numberP <|> listP) `P.sepBy` (P.char ',')) <* P.char ']')

blockP :: Parser (Packet, Packet)
blockP = do
  fst <- listP <* P.eol
  snd <- listP <* P.eol
  return (fst, snd)

inputP :: Parser [(Packet, Packet)]
inputP = P.someTill (P.try (blockP <* (P.char '\n')) <|> (P.try blockP)) P.eof

parse :: String -> [(Packet, Packet)]
parse = fromMaybe (error "Parsing failed") . P.parseMaybe inputP

cmp :: (Packet, Packet) -> Ordering
cmp tup = case tup of
  (List [], List []) -> EQ
  (List p1, List []) -> GT
  (List [], List p2) -> LT
  (List (x : xs), List (y : ys)) -> case cmp (x, y) of
    EQ -> cmp (List xs, List ys)
    rest -> rest
  (List p1, Number n) -> cmp (List p1, List [Number n])
  (Number n, List p2) -> cmp (List [Number n], List p2)
  (Number n1, Number n2) -> compare n1 n2

solve1 :: String -> IO ()
solve1 =
  print
    . sum
    . map fst
    . filter ((== LT) . snd)
    . zip ([1 ..] :: [Int])
    . map cmp
    . parse

solve2 :: String -> IO ()
solve2 =
  print
    . fromMaybe (error "Failed to find indices")
    . uncurry (liftM2 (*))
    . both (fmap (+ 1))
    . (findIndex (== delimiter1) &&& (findIndex (== delimiter2)))
    . sortBy (curry cmp)
    . (:) delimiter2
    . (:) delimiter1
    . uncurry (++)
    . unzip
    . parse
  where
    delimiter1 = List [List [Number 6]]
    delimiter2 = List [List [Number 2]]
