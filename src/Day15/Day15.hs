module Day15.Day15
  ( solve1
  , solve2
  ) where

import Misc

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

inp = unsafePerformIO $ readFile "src/Day15/input.txt"
ex =  unsafePerformIO $ readFile "src/Day15/ex.txt"

parse :: String -> [Sensor]
parse = fromJust . P.parseMaybe inputP

data Sensor = Sensor { sensorX :: Int
                     , sensorY :: Int
                     , beaconX :: Int
                     , beaconY :: Int
                     , manhattan :: Int
                     }
  deriving (Show, Eq)

data Rhombus = Rhomb { north :: (Int,Int)
                     , east :: (Int,Int)
                     , south :: (Int,Int)
                     , west :: (Int,Int)
                     , center :: (Int,Int)
                     , manh :: Int
                     }
    deriving (Show, Eq)


xBounds :: [Sensor] -> (Int,Int)
xBounds = go (maxBound,minBound)
  where
    go :: (Int,Int) -> [Sensor] -> (Int,Int)
    go (x,y) [] = (x,y)
    go (x,y) (z:zs) = let (Sensor sx sy bx by m) = z
                       in go (min (min (sx - m) (bx - m)) x, max (max (sx + m) (bx + m)) x) zs

sensorToRhombus :: Sensor -> Rhombus
sensorToRhombus (Sensor sx sy bx by m) = Rhomb (sx,sy-m) (sx+m,sy) (sx,sy+m) (sx-m,sy) (sx, sy) m

inRhombus :: (Int,Int) -> Rhombus -> Bool
inRhombus (x,y) r = getManhattan (x,y) (center r) <= manh r

inAny :: (Int,Int) -> [Rhombus] -> Bool
inAny point = any (inRhombus point)

-- 6070131, too high
-- 4390364, too low

solve1 :: String -> String
solve1 xs = show $ foldl' (\acc x -> if inAny x rhs then 1 + acc else acc) 0 range - 1
    where
      sensors = parse xs
      (from,to) = xBounds sensors
      range = [ (x,2000000) | x <- [ from .. to ] ]
      rhs = map sensorToRhombus sensors

solve2 :: String -> String
solve2 = const "unsolved"

-- First time megaparsec :)

type Parser = P.Parsec Void String


pointP :: Parser Int
pointP = do
    _ <- P.many (P.space *> P.letterChar <* P.space)
    _ <- P.char '='
    neg <- P.optional $ P.char '-'
    x <- L.decimal
    _ <- P.optional P.punctuationChar
    case neg of
        (Just _) -> pure $ x * (-1)
        _        -> pure x

sensorP :: Parser Sensor
sensorP = do
    sensX <- pointP
    sensY <- pointP
    beacX <- pointP
    beacY <- pointP
    _ <- P.eol
    pure $ Sensor sensX sensY beacX beacY (getManhattan (sensX,sensY) (beacX,beacY))

inputP :: Parser [Sensor]
inputP = do
    P.someTill sensorP P.eof
