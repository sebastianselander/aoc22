module Day7.Day7
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map qualified as M
import Data.Map.Strict qualified as MS

data Instruction = Cd String
                 | CdDotDot
                 | Ls
                 | DirInstr Directory
                 | FileInstr File
    deriving Show

data Directory = Dir String
    deriving (Show, Ord, Eq)

data File = File Int String
    deriving (Show, Ord, Eq)

type FS = M.Map Directory (Int,[Directory])

parse :: String -> [Instruction]
parse = map p . lines
  where
    p :: String -> Instruction
    p ("$ cd ..") = CdDotDot
    p ('$':' ': 'c':'d':' ' : xs) = Cd xs
    p ("$ ls") = Ls
    p ('d':'i':'r':' ' : xs) = DirInstr (Dir xs)
    p rest = let [size,name] = words rest
             in FileInstr (File (read size) name)

cm :: String -> FS
cm s = createMap [] (parse s) M.empty

createMap :: [String] -> [Instruction] -> FS -> FS
createMap stack [] fs = fs
createMap stack (x:xs) fs = case x of
    (Cd "/") -> createMap ("root" : stack) xs fs
    (Cd str) -> createMap (str : stack) xs fs
    CdDotDot -> createMap (tail stack) xs fs
    Ls -> createMap stack xs fs
    DirInstr (Dir name) -> createMap stack xs $ MS.insertWith (\(a,b) (x,y) -> (a+x, y ++ b)) (Dir (intercalate "/" $ reverse stack)) (0,[Dir $ intercalate "/" $ (reverse stack) ++ [name]]) fs
    FileInstr (File size name) -> createMap stack xs $ MS.insertWith (\(a,b) (x,y) -> (a+x, y ++ b)) (Dir (intercalate "/" $ reverse stack)) (size, []) fs

allAnswers :: FS -> [Int]
allAnswers fs = map (sum . fromJust . answer fs) $ M.keys fs

answer :: FS -> Directory -> Maybe [Int]
answer fs dir= do
    (size, dirs) <- M.lookup dir fs
    pure (size : (concatMap (fromMaybe [] . (answer fs)) dirs))

solve1 :: String -> IO ()
solve1 = print . sum . filter (<100000) . allAnswers . cm

solve2 :: String -> IO ()
solve2 str = print . head . dropWhile (\x -> totalsz - x > sz) . sort . allAnswers . cm $ str
    where
      sz = 70000000 - 30000000
      totalsz = head . sortBy (flip compare) . allAnswers . cm $ str
