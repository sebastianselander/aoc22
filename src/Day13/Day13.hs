module Day13.Day13
  ( solve1
  , solve2
  ) where

import Misc
import Data.Aeson
import Data.Text (unpack)
import Control.Arrow ((&&&))
import Data.Scientific (toBoundedInteger)
import Data.Foldable (toList)
import Data.ByteString.Char8 qualified as C

data Packet = V Int | List [Packet]
    deriving Eq

instance Show Packet where
  show (V n) = show n
  show (List xs) = show xs

instance FromJSON Packet where
  parseJSON (Number n) = pure $ V (fromJust $ toBoundedInteger n)
  parseJSON as@(Array arr) = withArray "List" (\a -> List . toList <$> traverse parseJSON a) as

parse :: String -> [Packet]
parse = map (fromJust . decodeStrict . C.pack) . filter (not . null) . lines

tup :: [a] -> [(a,a)]
tup []       = []
tup (x:y:ys) = (x,y) : tup ys

data Opt = Ok | Cont | Bad
    deriving Show

cmpOrd :: Opt -> Ordering
cmpOrd Bad = GT
cmpOrd Ok = LT
cmpOrd Cont = EQ

cmp :: (Packet,Packet) -> Opt
cmp tup = case tup of
    (List [], List []) -> Cont
    (List p1, List []) -> Bad
    (List [], List p2) -> Ok
    (List (x:xs), List (y:ys)) -> case cmp (x,y) of
        Bad -> Bad
        Cont -> cmp (List xs, List ys)
        Ok ->  Ok
    (List p1, V n) -> cmp (List p1,List [V n])
    (V n, List p2) -> cmp (List [V n],List p2)
    (V n1, V n2) -> case signum $ n2 - n1 of
        (-1) -> Bad
        0 -> Cont
        1 -> Ok

isOk :: Opt -> Bool
isOk Ok = True
isOk _  = False

solve1 :: String -> String
solve1 = show
       . sum
       . map fst
       . filter (isOk . snd)
       . zip ([1..] :: [Int])
       . map cmp
       . tup
       . parse

solve2 :: String -> String
solve2 = show
       . uncurry addM
       . bimap (fmap (+1)) (fmap (+1))
       . (findIndex (== (List [List [V 6]])) &&& (findIndex (== (List [List [V 2]]))))
       . sortBy (\a b -> cmpOrd (cmp (a,b)))
       . (\xs -> (List [List [V 6]]) : (List [List [V 2]]) : xs)
       . parse
    where
      addM (Just x) (Just y) = x * y
