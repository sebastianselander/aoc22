module Misc
  ( getInput
  , module Data.List.Split
  ) where

import Data.Char
import Data.List.Split

getInput :: String -> IO String
getInput file = readFile $ "/home/sebastian/Documents/git/aoc22/src/" <> file
