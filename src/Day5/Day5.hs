module Day5.Day5
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map qualified as M
import Control.Arrow (( &&& ))

data Instr = Instr { amount :: Int
                   , from :: Int
                   , to :: Int
                   }
    deriving Show

toInstr :: [String] -> [Instr]
toInstr []         = []
toInstr (a:b:c:xs) = Instr (read a) (read b) (read c) : toInstr xs
toInstr _          = error "failed to parse"

parseCrates :: String -> M.Map Int String
parseCrates = M.fromList
            . zip [1..9]
            . filter (not . null)
            . map (filter isUpper)
            . transpose
            . take 8
            . lines


parseInstr :: String -> [Instr]
parseInstr = toInstr
           . wordsBy (not . isDigit)
           . concat
           . drop 10
           . lines

runInstr :: (String -> String) -> Instr -> M.Map Int String -> M.Map Int String
runInstr f instr crates = updateCrate crates
    where
    froms = fromMaybe (error "fail") $ M.lookup (from instr) crates
    tos = fromMaybe (error "fail") $ M.lookup (to instr) crates
    updateCrate = M.insert (from instr) (drop (amount instr) froms) . M.insert (to instr) (f (take (amount instr) froms) ++ tos)

solve :: (String -> String) -> String -> String
solve f = map (head . snd) . M.toList . uncurry (foldl' (flip (runInstr f))) . (parseCrates &&& parseInstr)

solve1 :: String -> String
solve1 = solve reverse

solve2 :: String -> String
solve2 = solve id
