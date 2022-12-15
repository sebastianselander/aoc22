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

instance Ord Rhombus where
    compare r1 r2 = east r1 `compare` east r2


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

solve1 :: String -> String
solve1 xs = show $ foldl' (\acc x -> if inAny x rhs then 1 + acc else acc) 0 range - 1
    where
      sensors = parse xs
      (from,to) = xBounds sensors
      range = [ (x,2000000) | x <- [ from .. to ] ]
      rhs = map sensorToRhombus sensors


solve2 :: String -> String
solve2 = show
       . uncurry (+)
       . first (*4000000)
       . getPoint (0,0)
       . sortBy (flip compare)
       . map sensorToRhombus
       . parse

skip :: (Int,Int) -> Rhombus -> (Int,Int)
skip (x,y) rhombjävel = let dist = abs (y - (snd $ center rhombjävel))
                         in ((1 + (fst $ east rhombjävel)) - dist, y)

inside :: (Int,Int) -> [Rhombus] -> Maybe ((Int,Int),Rhombus)
inside p []     = Nothing
inside p (x:xs) = if (inRhombus p x) then Just (p,x) else inside p xs

getPoint :: (Int,Int) -> [Rhombus] -> (Int,Int)
getPoint p@(x,y) xs = if  x >= 4000000
                         then getPoint (0,y+1) xs
                         else if y >= 4000000
                                then error "Point not found"
                                else case inside p xs of
                                    Nothing -> p
                                    Just (p',r) -> getPoint (skip p' r) xs


-- Parser
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
    pure $ Sensor sensX sensY beacX beacY (getManhattan (sensX,sensY) (beacX,beacY))

inputP :: Parser [Sensor]
inputP = do
    P.someTill sensorP P.eof
