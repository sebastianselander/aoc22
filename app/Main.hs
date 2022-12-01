{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Misc qualified as M
import Day01.Day01 qualified as D1

main :: IO ()
main = do
    p1 <- D1.solve1
    p2 <- D1.solve2
    putStrLn ""
    putStrLn $ "Part one: " <> (show p1)
    putStrLn $ "Part two: " <> (show p2)
    return ()

