{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Misc qualified as M
import Day02.Day02 qualified as D

main :: IO ()
main = do
    p1 <- D.solve1
    p2 <- D.solve2
    putStrLn ""
    putStrLn $ "Part one: " <> p1
    putStrLn $ "Part two: " <> p2
    return ()

