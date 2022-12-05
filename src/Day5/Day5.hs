module Day5.Day5
  ( solve1
  , solve2
  ) where

import Misc
import System.IO.Unsafe (unsafePerformIO)
import Data.Map qualified as M

inp :: String
inp = unsafePerformIO $ readFile "src/Day5/input.txt"

-- parse :: String -> [String]
parseCrates :: String -> M.Map Int String
parseCrates = M.fromList . zip [1..9] . filter (not . null) . map (filter isUpper) . transpose . take 8 . lines

data Instr = Instr {
                     amount :: Int
                   , from :: Int
                   , to :: Int
                   }
    deriving Show

parseInstr :: String -> [Instr]
parseInstr = toInstr . wordsBy (not . isDigit) . concat . drop 10 . lines
    where
      toInstr :: [String] -> [Instr]
      toInstr []         = []
      toInstr (a:b:c:xs) = Instr (read a) (read b) (read c) : toInstr xs
      toInstr _          = error "failed to parse"

runInstr :: Instr -> M.Map Int String -> M.Map Int String
runInstr instr crates = crates'
    where
    (Just froms)  = M.lookup (from instr) crates
    (Just tos)  = M.lookup (to instr) crates
    crates'     = M.insert (to instr) ((take (amount instr) froms) ++ tos) $ M.insert (from instr) (drop (amount instr) froms) crates

-- solve1 :: String -> String
solve1 str = foldr runInstr (parseCrates str) (parseInstr str)

solve2 :: String -> String
solve2 = const "unsolved"
