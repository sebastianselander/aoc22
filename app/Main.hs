{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Misc qualified as M
import Day01.Day01 qualified as D

main :: IO ()
main = do
    p1 <- D.solve1
    p2 <- D.solve2
    putStrLn ""
    putStrLn $ "Part one: " <> (show p1)
    putStrLn $ "Part two: " <> (show p2)
    return ()

