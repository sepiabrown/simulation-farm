{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Parallel.Strategies
import System.Environment (getArgs)

import Logging
import MyFFI
import VecMat


composeLayers :: (MatD -> VecD -> VecD) -> [MatD] -> VecD -> VecD
composeLayers f ms v = foldl (flip f) v ms

main :: IO ()
main = flip evalStateT (0, Nothing) $ do

  startWatch "Arg Parsing"
  [nrow, ncol, ncol2, nd, nc, s] <- liftIO getArgs
  nCap        <- liftIO getNumCapabilities
  let
    numData = read nd :: Int
    numRows = read nrow :: Int
    numCols = read ncol :: Int
    numCols2 = read ncol2 :: Int
    numChunk = case read nc :: Int of
      0 -> nCap
      n -> n
    sizeChunk = numData `div` numChunk
  logging $ "(numRows, numCols, numCols2) = " ++ show (numRows, numCols, numCols2)
  logging $ "(numData, numChunk) = " ++ show (numData, numChunk)
  logging $ "nCap = " ++ show nCap
  logging $ "safety = " ++ s
  stopWatch

  startWatch "Non-Strict Initialization"
  let
    seeds = [1..numData]
    vs = map (`mkRandomVec` numCols2) seeds `using` parListChunk sizeChunk rdeepseq
    w1 = mkRandomMat 1 numRows numCols
    w2 = mkRandomMat 2 numCols numCols2
  stopWatch

  startWatch "Strict Initialization"
  vs' <- liftIO $ evaluate . force $ vs
  w1' <- liftIO $ evaluate . force $ w1
  w2' <- liftIO $ evaluate . force $ w2
  stopWatch

  startWatch "Computation"
  let
    res =
      if s == "s"
      then
        map (composeLayers mXvSafe [w2', w1']) vs' `using` parListChunk sizeChunk rdeepseq
      else
        map (composeLayers mXv [w2', w1']) vs' `using` parListChunk sizeChunk rdeepseq
  res' <- liftIO $ evaluate . force $ res
  stopWatch

  startWatch "Printing"
  let x = last $ vecToList (last res')
  logging $ "last element of the last vector is " ++ show x
  stopWatch
