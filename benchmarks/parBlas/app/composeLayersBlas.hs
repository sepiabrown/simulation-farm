import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Parallel.Strategies
import System.Environment (getArgs)

import VecMat
import MyBlas
import Logging

composeLayers :: (MatD -> VecD -> VecD) -> [MatD] -> VecD -> VecD
composeLayers f ms v = foldl (flip f) v ms

main :: IO ()
main = flip evalStateT (0, Nothing) $ do

  startWatch "Arg Parsing"
  [nData, nChunk, safety] <- liftIO getArgs
  nCap <- liftIO getNumCapabilities
  let
    numData = read nData :: Int
    numChunk = case read nChunk :: Int of
      0 -> nCap
      n -> n
    sizeChunk = numData `div` numChunk
  logging $ "numData = " ++ show numData
  logging $ "numChunk = " ++ show numChunk
  logging $ "safety = " ++ safety
  logging $ "numCapability = " ++ show nCap
  stopWatch

  startWatch "Non-Strict Initialization"
  let
    seeds = [1..numData]
    dims = [ 4, 10, 10, 20, 20, 2 ]

    vs = if safety == "s"
         then
           map (`mkRandomVecSafe` head dims) seeds `using` parListChunk sizeChunk rdeepseq
         else
           map (`mkRandomVec` head dims) seeds `using` parListChunk sizeChunk rdeepseq

    mats = if safety == "s"
           then
             zipWith3 mkRandomMatSafe [1..] (tail dims) dims
            else
             zipWith3 mkRandomMat [1..] (tail dims) dims
  stopWatch

  startWatch "Strict Initialization"
  vs' <- liftIO $ evaluate . force $ vs
  let ws = [head mats] ++ replicate 5 (mats !! 1) ++ [mats !! 2] ++ replicate 4 (mats !! 3) ++ [mats !! 4]
  ws' <- liftIO $ evaluate . force $ ws
  stopWatch

  startWatch "Computation"
  let
    res =
      if safety == "s"
      then
        map (composeLayers mXvSafe ws') vs' `using` parListChunk sizeChunk rdeepseq
      else
        map (composeLayers mXv ws') vs' `using` parListChunk sizeChunk rdeepseq
  res' <- liftIO $ evaluate . force $ res
  stopWatch

  startWatch "Printing"
  logging $ "Last result is " ++ show (last res')
  stopWatch
