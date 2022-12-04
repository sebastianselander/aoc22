{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day4.Day4 qualified as D

input :: IO FilePath
input = readFile $ "/home/sebastian/Documents/git/aoc22/src/" <> "Day4" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

