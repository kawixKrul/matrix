{-# LANGUAGE ConstrainedClassMethods #-}

module Multiplication (Multiplication (..)) where

import Data.Matrix (Matrix, elementwise, multStd)
import qualified MatrixShared as M

class Multiplication t where
  binet :: (Num t) => Matrix t -> Matrix t -> Matrix t
  standard :: (Num t) => Matrix t -> Matrix t -> Matrix t

instance Multiplication Double where
  binet = multBinet
  standard = multStd

multBinet :: (Num t) => Matrix t -> Matrix t -> Matrix t
multBinet a b
  | M.isOneElement a && M.isOneElement b = elementwise (*) a b
  | otherwise = M.joinBlocks c11 c12 c21 c22
  where
    (a11, a12, a21, a22) = M.splitMatrixCenter a
    (b11, b12, b21, b22) = M.splitMatrixCenter b

    c11 = elementwise (+) (multBinet a11 b11) (multBinet a12 b21)
    c12 = elementwise (+) (multBinet a11 b12) (multBinet a12 b22)
    c21 = elementwise (+) (multBinet a21 b11) (multBinet a22 b21)
    c22 = elementwise (+) (multBinet a21 b12) (multBinet a22 b22)