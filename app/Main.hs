{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Day25.Day25 qualified as D

input :: IO FilePath
input = readFile $ "src/" <> "Day25" <> "/input.txt"

main :: IO ()
main = do
    inp <- input
    putStrLn ""
    putStrLn "Part one: " >> D.solve1 inp
    putStrLn "Part two: " >> D.solve2 inp
    putStrLn ""
    return ()

