module Day6.Day6
  ( solve1
  , solve2
  ) where

import Misc

check1 :: Int -> String -> Int
check1 count (a:b:c:d:xs) =
    bool count (check1 (count+1) (b:c:d:xs) ((length $ nub [a,b,c,d]) == 4)

check2 :: Int -> String -> Int
check2 count (a:b:c:d:e:f:g:h:i:j:k:l:m:n:xs) =
    bool
        (check2 (count+1) (b:c:d:e:f:g:h:i:j:k:l:m:n:xs))
        count
        ((length $ nub [a,b,c,d,e,f,g,h,i,j,k,l,m,n]) == 14)

solve1 :: String -> String
solve1 = show . check1 4

solve2 :: String -> String
solve2 = show . check2 14
