module Day16.Day16
  ( solve1
  , solve2
  ) where

import Misc
import Control.Arrow ((&&&))
import Data.Map qualified as M
import Data.Map (Map)
import Data.PQueue.Min qualified as PQ

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

inp = unsafePerformIO $ readFile "src/Day16/ex.txt"


data Valve = Valve { name :: String -- Unnecessary...
                   , flowrate :: Int
                   , neighbors :: [String]
                   }
  deriving Show

-- parse :: String -> Map String Valve
parse = toMap . map (valve . (name &&& flow &&& neighbs)) . lines
    where
      name :: String -> String
      name = head . tail . words
      flow :: String -> Int
      flow = read . init . drop 5 . head . drop 4 . words
      neighbs :: String -> [String]
      neighbs = map (filter (/=',')) . drop 9 . words
      valve :: (String,(Int,[String])) -> Valve
      valve (a, (b, c)) = Valve a b c
      toMap :: [Valve] -> Map String Valve
      toMap = M.fromList . map (\v@(Valve n f ne) -> (n , v))

dijk :: Int -> Graph -> Queue -> Visited -> Int
dijk mins grph queue visited = case PQ.getMin queue of
    Nothing -> Nothing
    Just _ | mins == 0 = undefined
           | idx `S.member` visited -> undefined
           | otherwise -> undefined


solve1 :: String -> IO ()
solve1 = putStrLn . const "unsolved"

solve2 :: String -> IO ()
solve2 = putStrLn . const "unsolved"
