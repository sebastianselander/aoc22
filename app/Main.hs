{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day8.Day8 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day8" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

