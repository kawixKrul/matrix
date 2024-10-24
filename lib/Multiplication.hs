{-# LANGUAGE ConstrainedClassMethods #-}

module Multiplication (Multiplication (..), MultResult (..)) where

import Data.Matrix (Matrix, elementwise, multStd, ncols)
import qualified MatrixShared as M

data MultResult t = MultResult
  { result :: Matrix t,
    numAdditions :: Int,
    numMultiplications :: Int
  }

instance (Show t) => Show (MultResult t) where
  show (MultResult res addCount mulCount) =
    "Result Matrix:\n"
      ++ show res
      ++ "\nNumber of Additions: "
      ++ show addCount
      ++ "\nNumber of Multiplications: "
      ++ show mulCount

class Multiplication t where
  binet :: (Num t) => Matrix t -> Matrix t -> MultResult t
  strassen :: (Num t) => Matrix t -> Matrix t -> MultResult t
  standard :: (Num t) => Matrix t -> Matrix t -> Matrix t

instance Multiplication Double where
  binet = multBinet
  strassen = multStrassen
  standard = multStd

multBinet :: (Num t) => Matrix t -> Matrix t -> MultResult t
multBinet a b =
  let n = ncols a
      MultResult resultM add mul = multBinetRec a b
   in MultResult (M.toOriginalSize (resultM) n) add mul

multBinetRec :: (Num t) => Matrix t -> Matrix t -> MultResult t
multBinetRec a b
  | M.isOneElement a && M.isOneElement b =
      MultResult (elementwise (*) a b) 0 1
  | otherwise =
      let MultResult c11 add11 mul11 = getCxxMatrix a11 a12 b11 b21
          MultResult c12 add12 mul12 = getCxxMatrix a11 a12 b12 b22
          MultResult c21 add21 mul21 = getCxxMatrix a21 a22 b11 b21
          MultResult c22 add22 mul22 = getCxxMatrix a21 a22 b12 b22

          combinedMatrices = M.joinBlocks c11 c12 c21 c22

          totalAdds = add11 + add12 + add21 + add22
          totalMuls = mul11 + mul12 + mul21 + mul22
       in MultResult combinedMatrices totalAdds totalMuls
  where
    (a11, a12, a21, a22) = M.splitMatrixCenter a
    (b11, b12, b21, b22) = M.splitMatrixCenter b

getCxxMatrix :: (Num t) => Matrix t -> Matrix t -> Matrix t -> Matrix t -> MultResult t
getCxxMatrix a1 a2 b1 b2 =
  let MultResult res1 add1 mul1 = multBinetRec a1 b1
      MultResult res2 add2 mul2 = multBinetRec a2 b2
      addMatrices = elementwise (+) res1 res2
   in MultResult addMatrices (add1 + add2 + 1) (mul1 + mul2)

multStrassen :: (Num t) => Matrix t -> Matrix t -> MultResult t
multStrassen a b =
  let n = ncols a
      MultResult resultM add mul = multStrassenRec a b
   in MultResult (M.toOriginalSize (resultM) n) add mul

multStrassenRec :: (Num t) => Matrix t -> Matrix t -> MultResult t
multStrassenRec a b
  | M.isOneElement a && M.isOneElement b = MultResult (elementwise (*) a b) 0 1
  | otherwise =
      let (a11, a12, a21, a22) = M.splitMatrixCenter a
          (b11, b12, b21, b22) = M.splitMatrixCenter b
          elPlus = elementwise (+)
          elMinus = elementwise (-)

          p1 = multStrassenRec (elPlus a11 a22) (elPlus b11 b22)
          p2 = multStrassenRec (elPlus a21 a22) b11
          p3 = multStrassenRec a11 (elMinus b12 b22)
          p4 = multStrassenRec a22 (elMinus b21 b11)
          p5 = multStrassenRec (elPlus a11 a12) b22
          p6 = multStrassenRec (elMinus a21 a11) (elPlus b11 b12)
          p7 = multStrassenRec (elMinus a12 a22) (elPlus b21 b22)

          c11 = elPlus (result p1) (elPlus (elMinus (result p4) (result p5)) (result p7))
          c12 = elPlus (result p3) (result p5)
          c21 = elPlus (result p2) (result p4)
          c22 = elPlus (result p1) (elPlus (elMinus (result p3) (result p2)) (result p6))

          combinedMatrices = M.joinBlocks c11 c12 c21 c22

          totalAdds =
            numAdditions p1
              + numAdditions p2
              + numAdditions p3
              + numAdditions p4
              + numAdditions p5
              + numAdditions p6
              + numAdditions p7
              + 18

          totalMuls =
            numMultiplications p1
              + numMultiplications p2
              + numMultiplications p3
              + numMultiplications p4
              + numMultiplications p5
              + numMultiplications p6
              + numMultiplications p7
       in MultResult combinedMatrices totalAdds totalMuls
