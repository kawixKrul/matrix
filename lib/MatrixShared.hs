module MatrixShared (createNewMatrix, joinBlocks, isOneElement, splitMatrixCenter) where

import Data.Matrix (Matrix, fromList, ncols, nrows, splitBlocks, (<->), (<|>))
import System.Random (randomRIO)

generateRandomDouble :: IO Double
generateRandomDouble = randomRIO (0.00000001, 1.0)

generateRandomList :: Int -> IO [Double]
generateRandomList n = mapM (\_ -> generateRandomDouble) [1 .. n]

createNewMatrix :: Int -> Int -> IO (Matrix Double)
createNewMatrix rows cols = do
  values <- generateRandomList (rows * cols)
  return $ fromList rows cols values

joinBlocks :: (Num a) => Matrix a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
joinBlocks a b c d = (a <|> b) <-> (c <|> d)

isOneElement :: (Num a) => Matrix a -> Bool
isOneElement a = ncols a == 1 && nrows a == 1

splitMatrixCenter :: (Num a) => Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a)
splitMatrixCenter a = (a11, a12, a21, a22)
  where
    n = ncols a `div` 2
    m = nrows a `div` 2
    (a11, a12, a21, a22) = splitBlocks m n a