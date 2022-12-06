{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day6.Day6 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day6" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

