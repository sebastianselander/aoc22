module Day11.Day11
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map qualified as M

data Monkey = Monkey { starting :: [Integer]
                     , operation :: (Integer -> Integer)
                     , test :: Integer
                     , istrue :: Integer
                     , isfalse :: Integer
                     , inspected :: Integer
                     }

toMonkeys :: String -> M.Map Integer Monkey
toMonkeys = M.fromList . zip [0..] . map (parseMonkey . lines) . splitOn "\n\n"

parseMonkey :: [String] -> Monkey
parseMonkey [number,items, operation, test, true, false] =
    Monkey { starting = map read $ splitOn "," $ filter (/=' ') $ dropWhile (not . isDigit) items
            , operation = (toOp $ drop 23 operation)
            , test = (read $ drop 21 test)
            , istrue = (read $ filter isDigit true)
            , isfalse = (read $ filter isDigit false)
            , inspected = 0
            }
    where
      toOp :: String -> (Integer -> Integer)
      toOp "* old" = (^2)
      toOp ('+':' ':n) = (+ (read n))
      toOp ('*':' ':n) = (* (read n))
      toOp str        = error $ "Incorrect parsing of " <> str

addItem :: (Integer -> Integer) -> Integer -> Monkey -> Monkey
addItem modder item (Monkey s o t ist isf ins) = Monkey (s ++ [modder item]) o t ist isf ins

monkeyPlay :: (Integer -> Integer) -> Monkey -> (Maybe (Integer, Integer), Monkey)
monkeyPlay f monkey
  | null $ starting monkey   = (Nothing, monkey)
  | otherwise = let playItem = head $ starting monkey
                    op       = operation monkey
                    te       = (\x -> x `mod` test monkey == 0)
                    true     = istrue monkey
                    false    = isfalse monkey
                    ins      = inspected monkey
                 in let playItem' = f (op playItem)
                    in if te playItem'
                         then (Just (true,playItem'),
                               Monkey (tail $ starting monkey) op (test monkey) true false (ins+1)
                              )
                         else (Just (false, playItem'),
                               Monkey (tail $ starting monkey) op (test monkey) true false (ins+1)
                              )

playRound :: (Integer -> Integer)
          -> (Integer -> Integer)
          -> Integer
          -> M.Map Integer Monkey
          -> M.Map Integer Monkey
playRound modder f n m = case M.lookup n m of
    Nothing -> m
    (Just mnky) -> let (perhaps,monkey) = monkeyPlay f mnky
                   in case perhaps of
                       Nothing -> playRound modder f (n+1) (M.insert n monkey m)
                       Just (to,item) ->
                           playRound modder f n (adjustMnky modder (to,item) (M.insert n monkey m))

adjustMnky :: (Integer -> Integer)
           -> (Integer, Integer)
           -> M.Map Integer Monkey
           -> M.Map Integer Monkey
adjustMnky modder (to, item) m = M.adjust (addItem modder item) to m

solve1 :: String -> IO ()
solve1 = print
       . product
       . take 2
       . sortBy (flip compare)
       . map (inspected . snd)
       . M.toList
       . last
       . take 21
       . iterate (playRound id (flip div 3) 0)
       . toMonkeys

solve2 :: String -> IO ()
solve2 str = print
           . product
           . take 2
           . sortBy (flip compare)
           . map (inspected . snd)
           . M.toList
           . last
           . take 10001
           . iterate (playRound modder id 0) $ toMonkeys str
    where
      modder :: (Integer -> Integer)
      modder =  \x -> x `mod` (product . map (test . snd) . M.toList $ toMonkeys str)
