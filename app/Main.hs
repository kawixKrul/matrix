module Main where

import qualified MatrixShared as M
import qualified Multiplication as Mul

main :: IO ()
main = do
  a <- M.createNewMatrix 8 8
  b <- M.createNewMatrix 8 8
  let c = Mul.binet a b
  let d = Mul.standard a b
  print c
  print d