{-# LANGUAGE FlexibleContexts #-}
module Day16.Day16
  ( solve1
  , solve2
  ) where

import Misc
import Control.Arrow ((&&&))
import Data.Map qualified as M
import Data.Map (Map)
import Data.IORef
inp = unsafePerformIO $ readFile "src/Day16/ex.txt"


data Valve = Valve { name :: String
                   , flowrate :: Int
                   , neighbors :: [String]
                   }
  deriving Show

parse :: String -> Map String Valve
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

closeValve :: Valve -> Valve
closeValve (Valve nm fr nb) = (Valve nm 0 nb)

data MapWrapper = MapWrapper { x :: IORef (Map Int Int) }

algo :: MapWrapper -> Map String Valve -> String -> Int -> Int -> IO Int
algo mw graph cur acc 0 = return acc
algo mw graph cur acc mins = do
    memo <- getMap mw
    case M.lookup mins memo of
        Nothing -> do
            let current_flow = flowrate (graph M.! cur)
            move <- sequence $ map (\x -> algo mw graph x acc (mins - 1)) (neighbors $ graph M.! cur)
            open <- (algo mw (M.adjust closeValve cur graph) cur (acc + current_flow) (mins - 1))
            return (max open (maximum move))
        Just x -> pure x

makeMap :: Map Int Int -> IO MapWrapper
makeMap i = do
    iref <- newIORef i
    return $ MapWrapper iref

addToMap :: Int -> Int -> MapWrapper -> IO ()
addToMap from to (MapWrapper c) = do modifyIORef c (M.insert from to)

getMap :: MapWrapper -> IO (Map Int Int)
getMap (MapWrapper c) = do
    c' <- readIORef c
    return c'

solve1 :: String -> IO ()
solve1 str = do
    c <- makeMap M.empty
    n <- algo c (parse str) "AA" 0 10
    getMap c >>= print
    print n
solve2 :: String -> IO ()
solve2 = putStrLn . const "unsolved"
