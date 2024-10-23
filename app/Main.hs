module Main where

import qualified MatrixShared as M
import qualified Multiplication as Mul
import qualified Plotting as P
import qualified Utils as U

main :: IO ()
main = do
  let csvBinet = "mult_results_Binet.csv"
  let csvStrassen = "mult_results_Strassen.csv"

  -- Read and parse the CSV data
  huj <- P.readMatrixData csvBinet csvStrassen
  print huj