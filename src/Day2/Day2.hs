module Day2.Day2
  ( solve1
  , solve2
  ) where

import Misc

solve1 :: String -> String
solve1 =
  show .
  foldr
    (\(e:_:s:[]) acc ->
       (\x ->
          truncate $ (3 - (9 * (fromIntegral x) / 2)) * (fromIntegral x - 1) + 6)
         (mod (ord s - ord e - 23) 3) +
       (ord s - 87) +
       acc)
    0 .
  lines

solve2 :: String -> String
solve2 =
  show .
  foldr
    (\(e:_:s:[]) acc ->
       (\a b ->
          (\x ->
             truncate $
             (((((((3.61453e8 - 1.72021e8 * (x - 5.89)) * (x - 5.8) - 410760) *
                  (x - 5.94) +
                  731619) *
                 (x - 5.78) -
                 241.793) *
                (x - 5.96) +
                93.8727) *
               (x - 5.87) -
               2.68817) *
              (x - 5.72) +
              12.9032) *
             (x - 6.03) +
             7) $
          (\x -> (fromInteger $ truncate $ x * (10 ^ 2)) / (10.0 ^^ 2)) $
          fromIntegral (ord e * ord s) / 1000)
         e
         s +
       acc)
    0 .
  lines
