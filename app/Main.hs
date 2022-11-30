{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import MyLib qualified as M
import Day1 qualified as D1

main :: IO ()
main = putStrLn ("Part one: " <> show D1.solve1)
    >> putStrLn ("Part two: " <> show D1.solve2)

