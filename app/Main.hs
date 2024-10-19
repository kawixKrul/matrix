module Main where

import qualified MatrixShared as M
import qualified Multiplication as Mul

main :: IO ()
main = do
  let n = 6
  a <- M.createNewMatrix n n
  b <- M.createNewMatrix n n
  let c = Mul.binet a b
  let d = Mul.standard a b
  print c
  print d