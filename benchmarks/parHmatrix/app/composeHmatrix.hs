{-# LANGUAGE ImportQualifiedPost #-}

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Parallel.Strategies
import Numeric.LinearAlgebra ((#>), RandDist (Gaussian), randn)
import Numeric.LinearAlgebra qualified as L
import System.Environment (getArgs)

import Logging

linearLayer :: [ L.Matrix Double ] -> L.Vector Double
              -> L.Vector Double
linearLayer ws v = foldl (flip (#>)) v ws

mkRandomInput :: Int -> Int -> L.Vector Double
mkRandomInput dimX seed = L.randomVector seed Gaussian dimX

main :: IO ()
main = flip evalStateT (0, Nothing) $ do

  startWatch "Arg Parsing"
  [nData, nChunk] <- liftIO getArgs
  nCap <- liftIO getNumCapabilities
  let
    numData = read nData :: Int
    numChunk = case read nChunk :: Int of
      0 -> nCap
      n -> n
    sizeChunk = numData `div` numChunk
  logging $ "numData = " ++ show numData
  logging $ "numChunk = " ++ show numChunk
  logging $ "numCapability = " ++ show nCap
  stopWatch

  startWatch "Non-Strict Initialization"
  let
    seeds = [1..numData]
    dims = [ 4, 20, 20, 40, 40, 2 ]
    vs = map (mkRandomInput (head dims)) seeds `using` parListChunk sizeChunk rdeepseq
  mats <- liftIO $ zipWithM randn (tail dims) dims
  stopWatch

  startWatch "Strict Initialization"
  let ws = [head mats] ++ replicate 5 (mats !! 1) ++ [mats !! 2] ++ replicate 4 (mats !! 3) ++ [mats !! 4]
  vs' <- liftIO $ evaluate . force $ vs
  ws' <- liftIO $ evaluate . force $ ws
  stopWatch

  startWatch "Computation"
  res <- liftIO $ evaluate . force $
    (map (linearLayer ws') vs'
    `using` parListChunk sizeChunk rdeepseq)
  stopWatch

  startWatch "Printing"
  logging $ "Last result is " ++ show (last res)
  stopWatch
