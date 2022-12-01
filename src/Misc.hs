module MyLib where

import System.Directory

getInput :: String -> IO String
getInput file = readFile file

splitOn :: Char -> String -> [String]
splitOn c str =
  case break (== c) str of
    (a, c:b) -> a : splitOn c b
    (a, "") -> [a]
