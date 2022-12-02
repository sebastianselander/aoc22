{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Misc qualified as M
import Day2.Day2 qualified as D

input :: IO FilePath
input = readFile $ "/home/sebastian/Documents/git/aoc22/src/" <> "Day2" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

