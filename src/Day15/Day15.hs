module Day15.Day15
  ( solve1
  , solve2
  ) where

import Misc

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

parse :: String -> [Sensor]
parse = fromJust . P.parseMaybe inputP

type Sensor = (Int,Int,Int,Int)

data Rhombus = Rhomb { north     :: (Int, Int)
                     , east      :: (Int, Int)
                     , south     :: (Int, Int)
                     , west      :: (Int, Int)
                     , center    :: (Int, Int)
                     , manhattan :: Int }
  deriving (Show, Eq)

instance Ord Rhombus where
    compare r1 r2 = east r1 `compare` east r2

type Parser = P.Parsec Void String

pointP :: Parser Int
pointP = do
    _ <- P.many (P.space *> P.letterChar <* P.space)
    _ <- P.char '='
    x <- L.signed (pure ()) L.decimal
    _ <- P.optional P.punctuationChar
    pure x

sensorP :: Parser Sensor
sensorP = do
    sensX <- pointP
    sensY <- pointP
    beacX <- pointP
    beacY <- pointP
    _ <- P.eol
    pure (sensX,sensY,beacX,beacY)

inputP :: Parser [Sensor]
inputP = do
    P.someTill sensorP P.eof

xBounds :: [Sensor] -> (Int,Int)
xBounds = go (maxBound,minBound)
  where
    go :: (Int,Int) -> [Sensor] -> (Int,Int)
    go (x,y) [] = (x,y)
    go (x,y) (z:zs) = let (sx,sy,bx,by) = z
                          m = getManhattan (sx,sy) (bx,by)
                       in go (min (min (sx - m) (bx - m)) x, max (max (sx + m) (bx + m)) x) zs

toRhombus :: Sensor -> Rhombus
toRhombus (sx,sy,bx,by) =
    Rhomb (sx,sy-m) (sx+m,sy) (sx,sy+m) (sx-m,sy) (sx, sy) (getManhattan (sx,sy) (bx,by))
    where
      m = getManhattan (sx,sy) (bx,by)

inRhombus :: (Int,Int) -> Rhombus -> Bool
inRhombus (x,y) r = getManhattan (x,y) (center r) <= manhattan r

inAny :: (Int,Int) -> [Rhombus] -> Bool
inAny point = any (inRhombus point)

skip :: (Int,Int) -> Rhombus -> (Int,Int)
skip (x,y) r = let dist = abs (y - snd (center r))
                         in (1 + fst (east r) - dist, y)

inside :: (Int,Int) -> [Rhombus] -> Maybe ((Int,Int),Rhombus)
inside p []     = Nothing
inside p (x:xs) = if inRhombus p x then Just (p,x) else inside p xs

getPoint :: (Int,Int) -> [Rhombus] -> (Int,Int)
getPoint p@(x,y) xs
  | x >= 4000000 = getPoint (0,y+1) xs
  | y >= 4000000 = error "Point not found"
  | otherwise    = case inside p xs of
                        Nothing -> p
                        Just (p', r) -> getPoint (skip p' r) xs

solve1 :: String -> IO ()
solve1 xs = print
          . (+(-1))
          $ foldl' (\acc x -> if inAny x (map toRhombus (parse xs)) then 1 + acc else acc) 0 range
    where
      from, to :: Int
      (from,to) = xBounds (parse xs)
      range :: [(Int,Int)]
      range = zip [ from .. to ] (repeat 2000000)

solve2 :: String -> IO ()
solve2 = print
       . uncurry (+)
       . first (*4000000)
       . getPoint (0,0)
       . map toRhombus
       . parse
