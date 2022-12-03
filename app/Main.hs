{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Misc qualified as M
import Day3.Day3 qualified as D

input :: IO FilePath
input = readFile $ "/home/sebastian/Documents/git/aoc22/src/" <> "Day3" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

