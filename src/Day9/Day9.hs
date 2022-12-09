{-# LANGUAGE LambdaCase #-}
module Day9.Day9
  ( solve1
  , solve2
  ) where

import Misc

data Motion = L | U | R | D
    deriving Show

parse :: String -> [Motion]
parse = toMotion . lines
    where
      toMotion :: [String] -> [Motion]
      toMotion = \case
        [] -> []
        (x:xs) -> case x of
            ('L':' ':n) -> if read n == 1 then L : toMotion xs
                                          else L : toMotion (("L " ++ (show $ (read n) - 1)) : xs)
            ('R':' ':n) -> if read n == 1 then R : toMotion xs
                                          else R : toMotion (("R " ++ (show $ (read n) - 1)) : xs)
            ('U':' ':n) -> if read n == 1 then U : toMotion xs
                                          else U : toMotion (("U " ++ (show $ (read n) - 1)) : xs)
            ('D':' ':n) -> if read n == 1 then D : toMotion xs
                                          else D : toMotion (("D " ++ (show $ (read n) - 1)) : xs)

move :: (Int,Int) -> Motion -> (Int,Int)
move (x,y) R = (x+1,y)
move (x,y) L = (x-1,y)
move (x,y) U = (x,y+1)
move (x,y) D = (x,y-1)

tooFar :: (Int,Int) -> (Int,Int) -> Bool
tooFar (tx,ty) (hx,hy) = abs (hx - tx) > 1 || abs (hy - ty) > 1

hdmvnt :: [Motion] -> [(Int,Int)]
hdmvnt = (:) (0,0) . go (0,0)
  where
    go :: (Int,Int) -> [Motion] -> [(Int,Int)]
    go hd [] = []
    go hd (x:xs) = let newHead = move hd x
                   in newHead : go newHead xs

tailFollow :: (Int,Int) -> (Int,Int) -> (Int,Int)
tailFollow (tx,ty) (hx,hy) =
    -- Surely this can be written in a better way
    case (hx - tx, hy - ty) of

        (2,2) -> (tx+1,ty+1)
        (1,2) -> (tx+1,ty+1)
        (2,1) -> (tx+1,ty+1)

        (2,-2) -> (tx+1,ty-1)
        (1,-2) -> (tx+1,ty-1)
        (2,-1) -> (tx+1,ty-1)

        (0,2) -> (tx,ty+1)
        (0,-2) -> (tx,ty-1)
        (2,0) -> (tx+1,ty)
        (-2,0) -> (tx-1,ty)

        (-2,2) -> (tx-1,ty+1)
        (-1,2) -> (tx-1,ty+1)
        (-2,1) -> (tx-1,ty+1)

        (-2,-2) -> (tx-1,ty-1)
        (-1,-2) -> (tx-1,ty-1)
        (-2,-1) -> (tx-1,ty-1)

        _       -> (tx,ty)

tailMovement :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
tailMovement tl [cur] = if tooFar tl cur then [cur] else []
tailMovement tl (prev:cur:xs) = newTail : tailMovement newTail (cur:xs)
    where
      newTail = (tailFollow tl cur)

solve1 :: String -> String
solve1 = show
       . length
       . nub
       . tailMovement (0,0)
       . hdmvnt
       . parse

solve2 :: String -> String
solve2 = show
       . length
       . nub
       . last
       . take 10
       . iterate (tailMovement (0,0))
       . hdmvnt
       . parse
