{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified MatrixShared as M
import qualified Multiplication as Mul
import qualified Plotting as P
import qualified Utils as U

binetMul = False

strassenMul = False

main :: IO ()
main = do
  let csvBinet = "mult_results_Binet.csv"
  let csvStrassen = "mult_results_Strassen.csv"

  if
    | binetMul -> U.measureMultFunc Mul.binet "Binet"
    | strassenMul -> U.measureMultFunc Mul.strassen "Strassen"
    | otherwise -> putStrLn "No multiplication method selected, only plotting results"

  P.plotResult [csvBinet, csvStrassen]
