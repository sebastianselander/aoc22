{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day12.Day12 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day12" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

