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

toInstr :: [String] -> [Instr]
toInstr []         = []
toInstr (a:b:c:xs) = Instr (read a) (read b) (read c) : toInstr xs
toInstr _          = error "failed to parse"

runInstr1 :: Instr -> M.Map Int String -> M.Map Int String
runInstr1 instr crates = crates'
    where
    (Just froms)  = M.lookup (from instr) crates
    (Just tos)    = M.lookup (to instr) crates
    crates'       = M.insert (from instr) (drop (amount instr) froms) $ M.insert (to instr) (reverse (take (amount instr) froms) ++ tos) crates

runInstr2 :: Instr -> M.Map Int String -> M.Map Int String
runInstr2 instr crates = crates'
    where
    (Just froms)  = M.lookup (from instr) crates
    (Just tos)    = M.lookup (to instr) crates
    crates'       = M.insert (from instr) (drop (amount instr) froms) $ M.insert (to instr) ((take (amount instr) froms) ++ tos) crates

-- solve1 :: String -> String
solve1 str = map (head . snd) $ M.toList $ foldl' (\acc x -> runInstr1 x acc) (parseCrates str) (parseInstr str)

solve2 :: String -> String
solve2 str = map (head . snd) $ M.toList $ foldl' (\acc x -> runInstr2 x acc) (parseCrates str) (parseInstr str)
