{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Plotting (readMatrixData, plotResult) where

import Control.Monad
import Data.Either.Combinators
import Data.List
import Graphics.EasyPlot
import Text.CSV

readCsvRow :: Int -> [[String]] -> [Double]
readCsvRow row cells = map read $ map (\x -> x !! row) $ take (length cells - 1) ((tail . init) cells) :: [Double]

readMatrixData :: [[String]] -> Int -> [(Double, Double)]
readMatrixData cells row = zip (readCsvRow 0 cells) (readCsvRow row cells)

resultFiles :: [String]
resultFiles = ["plot_times.png", "plot_additions.png", "plot_multiplications.png"]

plotResultToFile :: [[(Double, Double)]] -> String -> IO Bool
plotResultToFile plotData outName = do
  plot
    (PNG outName)
    [ Data2D [Title "Binet", Style Lines, Color Blue] [] (plotData !! 0),
      Data2D [Title "Strassen", Style Lines, Color Red] [] (plotData !! 1)
    ]

plotResult :: [String] -> IO ()
plotResult files = do
  cells <- mapM (\f -> parseCSVFromFile f >>= return . fromRight') files
  let extractData row = map (\c -> readMatrixData c row) cells
  let plotData = map (\x -> extractData x) [1 .. 3]
  zipWithM_ plotResultToFile plotData resultFiles
