{-# LANGUAGE LambdaCase #-}
module Day9.Day9
  ( solve1
  , solve2
  ) where

import Prelude hiding (Either, Left, Right)
import System.IO.Unsafe (unsafePerformIO)
import Misc
import Debug.Trace

ex0 = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

ex1 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"



inp = unsafePerformIO $ readFile "src/Day9/input.txt"

data Motion = Left | Up | Right | Down
    deriving Show
parse :: String -> [Motion]
parse = toMotion . lines
    where
      toMotion :: [String] -> [Motion]
      toMotion = \case
        [] -> []
        (x:xs) -> case x of
            ('L':' ':n) -> if read n == 1 then Left : toMotion xs
                                          else Left : toMotion (("L " ++ (show $ (read n) - 1)) : xs)
            ('R':' ':n) -> if read n == 1 then Right : toMotion xs
                                          else Right : toMotion (("R " ++ (show $ (read n) - 1)) : xs)
            ('U':' ':n) -> if read n == 1 then Up : toMotion xs
                                          else Up : toMotion (("U " ++ (show $ (read n) - 1)) : xs)
            ('D':' ':n) -> if read n == 1 then Down : toMotion xs
                                          else Down : toMotion (("D " ++ (show $ (read n) - 1)) : xs)

move :: (Int,Int) -> Motion -> (Int,Int)
move (x,y) Right = (x+1,y)
move (x,y) Left = (x-1,y)
move (x,y) Up = (x,y+1)
move (x,y) Down = (x,y-1)

tailTooFar :: (Int,Int) -> (Int,Int) -> Bool
tailTooFar (tx,ty) (hx,hy) = abs (hx - tx) > 1 || abs (hy - ty) > 1

tailFollow :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int)
tailFollow tlx hprev hcur = bool tlx hprev (tailTooFar tlx hcur)

hdmvnt :: [Motion] -> [(Int,Int)]
hdmvnt = (:) (0,0) . headMovement (0,0)
  where
    headMovement :: (Int,Int) -> [Motion] -> [(Int,Int)]
    headMovement hd [] = []
    headMovement hd (x:xs) = newHead : headMovement newHead xs
        where
          newHead = move hd x

tailMovement :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
tailMovement tl [cur] = if tailTooFar tl cur then [cur] else []
tailMovement tl (prev:cur:xs) = newTail : tailMovement newTail (cur:xs)
    where
      newTail = (tailFollow tl prev cur)

tailFollow' :: (Int,Int) -> (Int,Int) -> (Int,Int)
tailFollow' (tx,ty) (hx,hy) =
    case (hx - tx, hy - ty) of
        (2,2) -> (tx+1,ty+1)
        (-2,-2) -> (tx-1,ty-1)
        (2,-2) -> (tx+1,ty-1)
        (-2,2) -> (tx-1,ty+1)

        (0,2) -> (tx,ty+1)
        (0,-2) -> (tx,ty-1)
        (2,0) -> (tx+1,ty)
        (-2,0) -> (tx-1,ty)

        (1,2) -> (tx+1,ty+1)
        (2,1) -> (tx+1,ty+1)
        (1,-2) -> (tx+1,ty-1)
        (-2,1) -> (tx-1,ty+1)

        (-1,2) -> (tx-1,ty+1)
        (2,-1) -> (tx+1,ty-1)
        (-1,-2) -> (tx-1,ty-1)
        (-2,-1) -> (tx-1,ty-1)
        _       -> (tx,ty)

tailMovement' :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
tailMovement' tl [cur] = if tailTooFar tl cur then [cur] else []
tailMovement' tl (prev:cur:xs) = newTail : tailMovement' newTail (cur:xs)
    where
      newTail = (tailFollow' tl cur)

sim = nub . last . take 10 . iterate (tailMovement' (0,0)) . hdmvnt . parse

solve1 :: String -> String
solve1 = show . length . nub . tailMovement (0,0) . hdmvnt . parse

solve2 :: String -> String
solve2 = show . length . sim
