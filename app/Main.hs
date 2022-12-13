{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day13.Day13 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day13" <> "/sam.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn $ "Part one: " <> D.solve1 inp
    putStrLn $ "Part two: " <> D.solve2 inp
    putStrLn ""
    return ()

