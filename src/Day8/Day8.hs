module Day8.Day8
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map qualified as M

type Index = (Int,Int)
row = 98
col = row

parse :: String -> M.Map Index Int
parse = mapify 0 . map (map digitToInt) . lines
    where
      mapify :: Int -> [[Int]] -> M.Map Index Int
      mapify _ [] = M.empty
      mapify n (x:xs) = M.union (help (n,0) x) (mapify (n+1) xs)

      help :: Index -> [Int] -> M.Map Index Int
      help (_,_) [] = M.empty
      help (i,j) (x:xs) = M.insert (i,j) x $ help (i,j+1) xs

check1 :: Index -> M.Map Index Int -> Int
check1 idx@(i,j) m = fromEnum $
                    m M.! idx > safeM (map ((M.!) m) brow) ||
                    m M.! idx > safeM (map ((M.!) m) srow) ||
                    m M.! idx > safeM (map ((M.!) m) bcol) ||
                    m M.! idx > safeM (map ((M.!) m) scol)
    where
      bcol = zip [i+1..row] (repeat j)
      scol = zip [0 .. i-1] (repeat j)
      brow = zip (repeat i) [j+1 .. col]
      srow = zip (repeat i) [0 .. j-1]
      safeM [] = -1
      safeM xs = maximum xs

check2 :: Index -> M.Map Index Int -> Int
check2 idx@(i,j) m = (length $ tkWh (\x -> x < cur) (map ((M.!) m) bcol)) *
                     (length $ tkWh (\x -> x < cur) (map ((M.!) m) scol)) *
                     (length $ tkWh (\x -> x < cur) (map ((M.!) m) brow)) *
                     (length $ tkWh (\x -> x < cur) (map ((M.!) m) srow))
    where
      tkWh :: (a -> Bool) -> [a] -> [a]
      tkWh pred [] = []
      tkWh pred (x:xs) = if pred x
                           then x : tkWh pred xs
                           else x : []

      cur = m M.! idx
      bcol = zip [i+1..row] (repeat j)
      scol = reverse $ zip [0 .. i-1] (repeat j)
      brow = zip (repeat i) [j+1 .. col]
      srow = reverse $ zip (repeat i) [0 .. j-1]

solve1 :: String -> String
solve1 str = show . sum $ map (\x -> check1 x (parse str)) (M.keys (parse str))

solve2 :: String -> String
solve2 str = show . maximum $ map (\x -> check2 x (parse str)) (M.keys (parse str))
