module Main where

import qualified MatrixShared as M
import qualified Multiplication as Mul

main :: IO ()
main = do
  let n = 8
  a <- M.createNewMatrix n
  b <- M.createNewMatrix n
  let c = Mul.binet a b
  let d = Mul.standard a b
  print c
  print d