module MatrixShared (createNewMatrix, joinBlocks, isOneElement, splitMatrixCenter, toOriginalSize) where

import Data.Matrix (Matrix, extendTo, fromList, ncols, nrows, splitBlocks, submatrix, (<->), (<|>))
import System.Random (randomRIO)

generateRandomDouble :: IO Double
generateRandomDouble = randomRIO (0.00000001, 1.0)

generateRandomList :: Int -> IO [Double]
generateRandomList n = mapM (\_ -> generateRandomDouble) [1 .. n]

createNewMatrix :: Int -> IO (Matrix Double)
createNewMatrix n = do
  values <- generateRandomList (n * n)
  return $ fromList n n values

joinBlocks :: (Num a) => Matrix a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
joinBlocks a b c d = (a <|> b) <-> (c <|> d)

isOneElement :: (Num a) => Matrix a -> Bool
isOneElement a = ncols a == 1 && nrows a == 1

splitMatrixCenter :: (Num a) => Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a)
splitMatrixCenter a = (a11, a12, a21, a22)
  where
    cols = ncols a
    rows = nrows a

    paddedA = padMatrixWithZeros a rows cols
    n = ncols paddedA `div` 2
    m = nrows paddedA `div` 2

    (a11, a12, a21, a22) = splitBlocks m n paddedA

extendZeros :: (Num t) => Int -> Int -> Matrix t -> Matrix t
extendZeros = extendTo 0

padMatrixWithZeros :: (Num t) => Matrix t -> Int -> Int -> Matrix t
padMatrixWithZeros a rows cols
  | odd rows && odd cols = extendZeros (rows + 1) (cols + 1) a
  | odd rows = extendZeros (rows + 1) cols a
  | odd cols = extendZeros rows (cols + 1) a
  | otherwise = a

toOriginalSize :: (Num t) => Matrix t -> Int -> Matrix t
toOriginalSize a n = submatrix 1 n 1 n a