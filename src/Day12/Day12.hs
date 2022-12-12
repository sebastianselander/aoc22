module Day12.Day12
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map qualified as M
import Data.PQueue.Min qualified as PQ
import Data.Set qualified as S

type Index = (Int,Int)
type Graph = M.Map Index Char
type Queue = PQ.MinQueue (Int,Index)
type Visited = S.Set Index

inp = unsafePerformIO $ readFile "src/Day12/input.txt"
ex = unsafePerformIO $ readFile "src/Day12/ex.txt"



parse :: String -> Graph
parse = M.fromList . concat . indexify 0 . lines
    where
      indexify :: Int -> [[a]] -> [[((Int,Int),a)]]
      indexify _ [] = []
      indexify n (x:xs) = (zip (zip (repeat n) [0..]) x) : indexify (n+1) xs

neighborIndex :: Index -> [Index]
neighborIndex (row,col) = [ (row, col - 1)
                      , (row, col + 1)
                      , (row - 1, col)
                      , (row + 1, col)
                      ]

getNeighbors :: Index -> Graph -> [Index]
getNeighbors idx m = map fst
                   . filter (reachable cur . snd)
                   . map tupFJ
                   . filter (isJust . snd)
                   . zip (neighborIndex idx)
                   . map (flip M.lookup m)
                   $ neighborIndex idx
    where
      tupFJ :: (a,Maybe b) -> (a,b)
      tupFJ (a, Nothing) = error "tupFJ failed"
      tupFJ (a, Just x) = (a,x)
      cur = m M.! idx
      reachable 'S' d = reachable 'a' d
      reachable c 'E' = reachable c 'z'
      reachable c d = (fromEnum c - fromEnum d) >= 0 || (fromEnum d - fromEnum c) == 1

dijk :: Graph -> Queue -> Visited -> Int
dijk grph queue visited = case PQ.getMin queue of
    Nothing -> 0
    Just (dist,idx) | grph M.! idx == 'E' -> dist
                    | idx `S.member` visited -> dijk grph (PQ.deleteMin queue) visited
                    | otherwise ->  dijk
                                    grph
                                    (foldl' (flip PQ.insert) queue (zip (repeat (dist + 1)) (getNeighbors idx grph)))
                                    (S.insert idx visited)
    where
      ctrace x = trace (show x) x

solve1 :: String -> String
solve1 str = show $ dijk grph (PQ.singleton (0,(20,0))) S.empty
    where
      grph = parse str

solve2 :: String -> String
solve2 str = show $ minimum $ filter (> 0) $ map (\x -> dijk grph (PQ.singleton (0,x)) S.empty) allStarts
    where
      grph = parse str
      allStarts :: [Index]
      allStarts = map fst $ filter (\(a,b) -> b=='a') $ M.toList grph

