{-# LANGUAGE OverloadedStrings #-}

module Plotting (readMatrixData) where

import Data.Either.Combinators
import Data.List
import Graphics.EasyPlot
import Text.CSV

readMatrixData fileName fileName2 = do
  eitherErrorOrCells <- parseCSVFromFile fileName
  let cells = fromRight' eitherErrorOrCells
  let ns = map read $ map (\x -> x !! 0) (take (length cells - 1) ((tail . init) cells)) :: [Double]
  let times = map read $ map (\x -> x !! 1) (take (length cells - 1) ((tail . init) cells)) :: [Double]
  let timesZipped = zip ns times
  eitherErrorOrCells2 <- parseCSVFromFile fileName2
  let cells2 = fromRight' eitherErrorOrCells2
  let times2 = map read $ map (\x -> x !! 1) (take (length cells2 - 1) ((tail . init) cells2)) :: [Double]
  let timesZipped2 = zip ns times2
  plot (PNG "kurwo.png") [Data2D [Title "Binet", Style Lines, Color Blue] [] timesZipped, Data2D [Title "Strassen", Style Lines, Color Red] [] timesZipped2]
