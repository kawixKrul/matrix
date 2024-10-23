module Utils (measureMultFunc) where

import Control.DeepSeq (deepseq)
import Data.Matrix (Matrix)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import MatrixShared (createNewMatrix)
import Multiplication (MultResult (..))
import System.CPUTime (getCPUTime)
import System.IO (appendFile, writeFile)
import Text.Printf (printf)

measureMultFunc :: (Matrix Double -> Matrix Double -> MultResult Double) -> String -> IO ()
measureMultFunc mulFunc funcName = do
  let fileName = "mult_results_" ++ funcName ++ ".txt"
  writeFile fileName "Size,Time,Adds,Muls\n"
  let measureTimeWithFunc = measureMultiplicationTime mulFunc
  mapM_ (measureTimeWithFunc fileName) [1 .. 100]

measureMultiplicationTime :: (Matrix Double -> Matrix Double -> MultResult Double) -> String -> Int -> IO ()
measureMultiplicationTime mulFunc fileName n = do
  matrixA <- createNewMatrix n
  matrixB <- createNewMatrix n
  startTime <- getCurrentTime
  let MultResult resultMatrix totalAdds totalMuls = mulFunc matrixA matrixB
  resultMatrix `deepseq` return ()
  endTime <- getCurrentTime
  putStrLn $ "For n:" ++ show n ++ " Start time: " ++ show startTime ++ "\nEnd time: " ++ show endTime
  let executionTime = realToFrac (diffUTCTime endTime startTime) :: Double
  appendFile fileName $ printf "%d,%f,%d,%d\n" n executionTime totalAdds totalMuls
