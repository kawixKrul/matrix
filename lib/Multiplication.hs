{-# LANGUAGE ConstrainedClassMethods #-}

module Multiplication (Multiplication (..)) where

import Data.Matrix (Matrix, elementwise, multStd, ncols)
import qualified MatrixShared as M

class Multiplication t where
  binet :: (Num t) => Matrix t -> Matrix t -> Matrix t
  standard :: (Num t) => Matrix t -> Matrix t -> Matrix t

instance Multiplication Double where
  binet = multBinet
  standard = multStd

multBinet :: (Num t) => Matrix t -> Matrix t -> Matrix t
multBinet a b = M.toOriginalSize (multBinetRec a b) n where n = ncols a

multBinetRec :: (Num t) => Matrix t -> Matrix t -> Matrix t
multBinetRec a b
  | M.isOneElement a && M.isOneElement b = elementwise (*) a b
  | otherwise =
      M.joinBlocks
        (getCxxMatrix a11 a12 b11 b21)
        (getCxxMatrix a11 a12 b12 b22)
        (getCxxMatrix a21 a22 b11 b21)
        (getCxxMatrix a21 a22 b12 b22)
  where
    (a11, a12, a21, a22) = M.splitMatrixCenter a
    (b11, b12, b21, b22) = M.splitMatrixCenter b
    getCxxMatrix x y w z = elementwise (+) (multBinet x w) (multBinet y z)
