module Day14.Day14
  ( solve1
  , solve2
  ) where

import Misc
import Data.Set (Set)
import Data.Set qualified as S

type Index = (Int,Int)

wall :: Index -> Index -> [Index]
wall src@(srcx,srcy) (dstx,dsty) =
    let dx = dstx - srcx
        dy = dsty - srcy
     in map (addT src) $ zip (stretch dx) (stretch dy)

stretch :: Int -> [Int]
stretch n = case signum n of
    1 -> [ 0 .. n]
    0 -> repeat 0
    (-1) -> [0, (-1) .. n]

createWalls :: Set Index -> [Index] -> Set Index
createWalls s (x:y:ys) = createWalls (foldl' (flip S.insert) s (wall x y)) (y:ys)
createWalls s _        = s

allWalls :: Set Index -> [[Index]] -> Set Index
allWalls s = foldl' createWalls S.empty

parse :: String -> Set Index
parse = allWalls S.empty . map ((map tuplify) . splitOn " -> ") . lines
    where
      tuplify :: String -> Index
      tuplify = both read . head . blockOf2 . splitOn ","

findFloor :: Set Index -> Int
findFloor s = maximum
. map (snd . fromJust)
. filter isJust
. map (flip S.lookupLE s)
$ zip [400 .. 500] (repeat 1000)

sand1 :: Int -> Index -> Set Index -> Int
sand1 floor (x,y) set
  | y >= floor = 0
sand1 floor (x,y) set =
    case map (flip S.member set) ([(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)] :: [Index]) of
        [_, False, _] -> sand1 floor (x, y + 1) set
        [False, _, _] -> sand1 floor (x - 1, y + 1) set
        [_, _, False] -> sand1 floor (x + 1, y + 1) set
        [True,True,True] ->  1 + sand1 floor (500,0) (S.insert (x,y) set)

sand2 :: Int -> Index -> Set Index -> Int
sand2 floor (500,0) set | (500,0) `S.member` set = 0
sand2 floor (x, y) set  | floor == y = 1 + sand2 floor (500,0) (S.insert (x,y) set)
sand2 floor (x,y) set =
    case map (flip S.member set) ([(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)] :: [Index]) of
        [_, False, _] -> sand2 floor (x, y + 1) set
        [False, _, _] -> sand2 floor (x - 1, y + 1) set
        [_, _, False] -> sand2 floor (x + 1, y + 1) set
        [True,True,True] ->  1 + sand2 floor (500,0) (S.insert (x,y) set)

solve1 :: String -> String
solve1 str = show $ sand1 (findFloor set) (500,0) set
    where
      set = parse str

solve2 :: String -> String
solve2 str = show $ sand2 (1 + findFloor set) (500,0) set
    where
      set = parse str
