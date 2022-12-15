module Day14.Day14
  ( solve1
  , solve2
  ) where

import Misc
import Data.HashSet (HashSet)
import Data.HashSet qualified as S

type Index = (Int,Int)
inp = unsafePerformIO $ readFile "src/Day14/input.txt"

stretch :: Int -> [Int]
stretch n = case signum n of
    1 -> [ 0 .. n ]
    0 -> repeat 0
    (-1) -> [ 0, (-1) .. n ]

wall :: Index -> Index -> [Index]
wall src@(srcx,srcy) (dstx,dsty) =
    map (addTuples src) $ zip (stretch (dstx - srcx)) (stretch (dsty - srcy))

createWalls :: HashSet Index -> [Index] -> HashSet Index
createWalls s (x:y:ys) = createWalls (foldl' (flip S.insert) s (wall x y)) (y:ys)
createWalls s _        = s

allWalls :: HashSet Index -> [[Index]] -> HashSet Index
allWalls s = foldl' createWalls S.empty

parse :: String -> HashSet Index
parse = allWalls S.empty . map ((map tuplify) . splitOn " -> ") . lines
    where
      tuplify :: String -> Index
      tuplify = both read . head . blockOf2 . splitOn ","

findFloor :: HashSet Index -> Int
findFloor = maximum . map snd . S.toList

simulate :: Bool -> Int -> Index -> HashSet Index -> Int
simulate b flr (500,0) set | (500,0) `S.member` set = 0
simulate b flr (x,y) set   | b && flr + 1 == y = 1 + simulate b flr (500,0) (S.insert (x,y) set)
                           | not b && y >= flr = 0
simulate b flr (x,y) set =
    case map (flip S.member set) ([ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1) ] :: [Index]) of
        [_, False, _] -> simulate b flr (x, y + 1) set
        [False, _, _] -> simulate b flr (x - 1, y + 1) set
        [_, _, False] -> simulate b flr (x + 1, y + 1) set
        _             ->  1 + simulate b flr (500,0) (S.insert (x,y) set)

solver :: Bool -> HashSet Index -> Int
solver b set = simulate b (findFloor set) (500,0) set

solve1 :: String -> String
solve1 = show . solver False . parse

solve2 :: String -> String
solve2 = show . solver True . parse
