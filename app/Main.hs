{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day10.Day10 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day10" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

