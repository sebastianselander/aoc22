{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day11.Day11 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day11" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

