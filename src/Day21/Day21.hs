module Day21.Day21
  ( solve1
  , solve2
  ) where

import Misc
import Data.Map (Map)
import Data.Map qualified as M
import Control.Monad (liftM2)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

inp = unsafePerformIO $ readFile "src/Day21/input.txt"

type Parser = P.Parsec Void String

data Value = Number Integer | Variable String
    deriving Show

data Expr = Var Value | Expr Value Op Value
    deriving Show

data Op = Mul | Div | Add | Sub | Equal
    deriving Show

opP :: Parser Op
opP = do
    op <- P.char '+' <|>
          P.char '-' <|>
          P.char '/' <|>
          P.char '*'
    case op of
       '*' -> return Mul
       '/' -> return Div
       '+' -> return Add
       '-' -> return Sub

varP :: Parser Value
varP = do
     Variable <$> (P.some P.letterChar) <|> Number <$> L.decimal

varExprP :: Parser Expr
varExprP = (do
    var1 <- varP
    op <- ((P.many $ P.char ' ') *> opP <* (P.many $ P.char ' '))
    var2 <- varP
    return (Expr var1 op var2))

numExprP :: Parser Expr
numExprP = Var . Number <$> L.decimal

lineP :: Parser (String,Expr)
lineP = do
    var <- P.some P.letterChar
    _ <- P.char ':'
    _ <- P.many (P.char ' ')
    expr <- numExprP <|> varExprP
    _ <- P.eol
    return (var,expr)

inputP :: Parser [(String,Expr)]
inputP = P.someTill lineP P.eof

parse :: String -> Maybe (Map String Expr)
parse = fmap M.fromList . P.parseMaybe inputP

-- End of parsing
---------------------------------------------------------------------------------------------------

opToFun :: Op -> (Integer -> Integer -> Integer)
opToFun Add = (+)
opToFun Sub = (-)
opToFun Mul = (*)
opToFun Div = div
opToFun Equal = \a b -> case compare a b of
    GT -> 1
    EQ -> 0
    LT -> (-1)

evalV :: Map String Expr -> Value -> Maybe Integer
evalV m = \case
    Number i -> pure i
    Variable v -> M.lookup v m >>= evalE m

evalE :: Map String Expr -> Expr -> Maybe Integer
evalE m = \case
    Var v -> evalV m v
    Expr v1 o v2 -> liftM2 (opToFun o) (evalV m v1) (evalV m v2)

fixMap :: Map String Expr -> Map String Expr
fixMap m = M.adjust (\(Expr v1 o v2) -> Expr v1 Equal v2) "root" m

-- Assumes map is adjusted to be correct prior
makeGuess :: Integer -> Map String Expr -> Maybe Integer
makeGuess n m = evalV (M.insert "humn" (Var . Number $ n) m) (Variable "root")

-- Binary search
findGuess :: Map String Expr -> Maybe Integer
findGuess m = go (hella_high `div` 2) (hella_high `div` 4) m
  where
    hella_high = 10000000000000000000000000000
    go :: Integer -> Integer -> Map String Expr -> Maybe Integer
    go n dn m = case makeGuess n m of
       Just 1 -> go (n + dn) (dn `div` 2) m
       Just 0 -> pure n
       Just (-1) -> go (n - dn) (dn `div` 2) m
       _ -> Nothing

-- Run `ghci input.hs` followed by `root`, answer for part 1 done
solve1 :: String -> IO ()
solve1 str = print $ (flip evalV (Variable "root")) =<< parse str

solve2 :: String -> IO ()
solve2 = print . fmap (findGuess . fixMap) . parse
