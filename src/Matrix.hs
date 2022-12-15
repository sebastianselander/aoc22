module Matrix where

import Data.Vector (Vector, MVector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

type Row = Int
type Col = Int
data Matrix a = Matrix { getVec :: (Vector a)
                       , height :: Int
                       , width :: Int
                       }
                       -- deriving Show

instance Foldable Matrix where
  foldr f acc m@(Matrix _ _ _) = foldr f acc (getVec m)

instance Show a => Show (Matrix a) where
    show m = show $ map (flip unsafeRow m) [0 .. ((height m) - 1)]

instance Functor Matrix where
  fmap f m@(Matrix _ h w) = Matrix (fmap f (getVec m)) h w

prettyPrint :: Show a => Matrix a -> IO ()
prettyPrint m@(Matrix vec h w) = mapM_ print $ map (flip unsafeRow m) [0 .. h - 1]

null :: Matrix a -> Bool
null (Matrix vec w h) = V.null vec

-- TODO: Fix it to be O(1). Just have to be sure everything works first
size :: Matrix a -> Int
size m = V.length (getVec m) --width m * height m

singleton :: a -> Row -> Col -> Matrix a
singleton v w h = Matrix (V.replicate (w * h) v) h w

-- initInc :: Int -> Row -> Col -> Matrix Int
initInc v row col = Matrix (V.fromList [v, v + 1 .. (col * row)]) row col

unsafeGet :: Row -> Col -> Matrix a -> a
unsafeGet x y m@( Matrix vec w h )
  | x < 0     = error $ "Row: " <> show x <> " does not exist"
  | y < 0     = error $ "Col: " <> show y <> " does not exist"
  | x * h + y * w >= size m = error "Index out of range"
  | otherwise = vec V.! (x * h + y * w)

get :: Int -> Int -> Matrix a -> Maybe a
get x y m@( Matrix vec w h ) = vec V.!? (x * h + y)

unsafeRow :: Int -> Matrix a -> Vector a
unsafeRow index (Matrix vec h w)
  | index >= h = error $ "Row index: " <> show index <> " out of range"
  | otherwise  = V.slice (index * h) w vec

row :: Int -> Matrix a -> Maybe (Vector a)
row index (Matrix vec h w)
  | index >= h = Nothing
  | otherwise  = Just $ V.slice (index * h) w vec

unsafeCol :: Int -> Matrix a -> Vector a
unsafeCol index m@(Matrix vec h w)
  | index >= h = error $ "Col index: " <> show index <> " out of range"
  | otherwise  = V.fromList $ map ((V.!) vec) ([index, index + w .. (V.length vec) - 1] :: [Int])

col :: Int -> Matrix a -> Maybe (Vector a)
col index m@(Matrix vec h w)
  | index >= h = Nothing
  | otherwise  = Just $ V.fromList $ map ((V.!) vec) ([index, index + w .. (V.length vec) - 1] :: [Int])

update :: a -> Row -> Col -> Matrix a -> Matrix a
update v row col (Matrix vec h w) = Matrix (vec V.// [((row * h) + col, v)]) h w


-- | O(m + n)
-- | Concat matrix below. Must be of same width
(#) :: Matrix a -> Matrix a -> Matrix a
(#) m1 m2 = Matrix ((getVec m1) V.++ (getVec m2)) (height m1 + height m2) (width m1)

-- DO NOT USE
-- INCREDIBLY BUGGY
