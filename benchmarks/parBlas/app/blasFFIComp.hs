{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad.State (evalStateT)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (getNumCapabilities)
import System.Environment (getArgs)

import Logging
import VecMat
import MyFFI qualified as F
import MyBlas qualified as B

main :: IO ()
main = flip evalStateT (0, Nothing) $ do

  startWatch "Arg Parsing"
  [nrow, ncol, ncol2] <- liftIO getArgs
  nCap        <- liftIO getNumCapabilities
  let
    numRows = read nrow :: Int
    numCols = read ncol :: Int
    numCols2 = read ncol2 :: Int
  logging $ "(numRows, numCols, numCols2) = " ++ show (numRows, numCols, numCols2)
  logging $ "nCap = " ++ show nCap
  stopWatch

  startWatch "Constructing and printing matrix"
  let
    w1 = matFromList 4 2 [1::Double,2,3,4,5,6,7,8]
    w2 = matFromList 2 3 [1.2,2.1,3.2,2.0,5.5,0.9]
  logging $ "w1 = " ++ show w1
  logging $ "w2 = " ++ show w2
  stopWatch

  startWatch "Blas"
  let
    br = B.mXm w1 w2
  logging $ "result = " ++ show br
  stopWatch

  startWatch "FFI"
  let
    fr = F.mXm w1 w2
  logging $ "result = " ++ show fr
  stopWatch

  startWatch "Constructing large random matrix"
  let
    rm1 = mkRandomMat 1 numRows numCols
    rm2 = mkRandomMat 2 numCols numCols2
  rm1' <- liftIO $ evaluate . force $ rm1
  rm2' <- liftIO $ evaluate . force $ rm2
  logging $ show rm1'
  logging $ show rm2'
  stopWatch

  startWatch "Blas"
  let
    brr = B.mXm rm1' rm2'
  logging $ show brr
  -- logging $ "last element is = " ++ show (last (matToList brr))
  stopWatch

  startWatch "FFI"
  let
    frr = F.mXm rm1' rm2'
  logging $ show frr
  -- logging $ "last element is = " ++ show (last (matToList frr))
  stopWatch
