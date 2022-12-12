module Matrix where

import Data.Vector (Vector, MVector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

data Matrix a = Matrix { width :: Int
                       , height :: Int
                       , (Vector a)
                       ,

-- width :: Matrix a -> Int
-- width (Matrix vec) = V.length (vec V.! 0)

-- height :: Matrix a -> Int
-- height (Matrix vec) = V.length vec

-- null :: Matrix a -> Bool
-- null (Matrix vec) = V.null vec

-- unsafeGet :: Int -> Int -> Matrix a -> a
-- unsafeGet x y (Matrix vec) = (vec V.! y) V.! x

-- -- Can't figure out how to write using fmap, some random stupid error, cba
-- get :: Int -> Int -> Matrix a -> Maybe a
-- get x y (Matrix vec) = case (vec V.!? y) of
--     Nothing -> Nothing
--     Just vec' -> vec' V.!? x

-- unsafeRow :: Int -> Matrix a -> Vector a
-- unsafeRow index (Matrix vec) = vec V.! index

-- row :: Int -> Matrix a -> Maybe (Vector a)
-- row index (Matrix vec) = vec V.!? index

-- unsafeCol :: Int -> Matrix a -> Vector a
-- unsafeCol index (Matrix vec)
