import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Parallel.Strategies (using, parListChunk, rdeepseq)
import System.Environment (getArgs)

import Logging
import MyFFI
import VecMat

composeLayers :: (MatD -> VecD -> VecD) -> [MatD] -> VecD -> VecD
composeLayers f ms v = foldl (flip f) v ms

layerTanhSafe :: MatD -> VecD -> VecD
layerTanhSafe w i = vectorMapRSafe Tanh v
  where
    v = mXvSafe w i

layerTanh :: MatD -> VecD -> VecD
layerTanh w i = vectorMapR Tanh v
  where
    v = mXv w i

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
    dims = [ 4, 20, 20, 40, 40, 2 ]
    vs = if safety == "s"
         then
           map (`mkRandomVecSafe` head dims) seeds `using` parListChunk sizeChunk rdeepseq
         else
           map (`mkRandomVec` head dims) seeds `using` parListChunk sizeChunk rdeepseq

    ws = if safety == "s"
         then
           zipWith3 mkRandomMatSafe [1..] (tail dims) dims
         else
           zipWith3 mkRandomMat [1..] (tail dims) dims
  stopWatch

  startWatch "Strict Initialization"
  vs' <- liftIO $ evaluate . force $ vs
  ws' <- liftIO $ evaluate . force $ ws
  stopWatch

  startWatch "Computation"
  let res = if safety == "s"
            then
              map (composeLayers layerTanhSafe ws') vs' `using` parListChunk sizeChunk rdeepseq
            else
              map (composeLayers layerTanh ws') vs' `using` parListChunk sizeChunk rdeepseq
  res' <- liftIO $ evaluate . force $ res
  stopWatch

  -- startWatch "Summation"
  -- s <- liftIO $ evaluate . force $ L.sumElements . mconcat $ res
  -- stopWatch

  startWatch "Printing"
  logging $ "Last result is " ++ show (last res')
  stopWatch
