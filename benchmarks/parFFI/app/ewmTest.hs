{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
import Foreign.Ptr
import GHC.IO (unsafePerformIO)
import Control.Monad (when)
import Prelude hiding (elem)
import VecMat
import Logging
import Control.Monad.State (evalStateT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Control.Concurrent (getNumCapabilities)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (using, parListChunk, rdeepseq, Strategy)


foreign import ccall unsafe "ewm" ewmc :: Int -> Ptr Double -> Ptr Double -> IO ()

ewmultList :: [Double] -> [Double] -> [Double]
ewmultList = zipWith (*)

vecConcat :: [Vec Double] -> Vec Double
vecConcat vs = vecFromList $ concat (fmap vecToList vs)

ewmult :: Vec Double -> Vec Double -> Vec Double
ewmult v1 v2 = unsafePerformIO $ do
  let n1 = dim v1
      n2 = dim v2
  when (n1 /= n2) $ error "inconsitent dimensions in element wise vector multiplication"
  _ <- unsafeVecWith v1 $ \fp1 ->
         unsafeVecWith v2 $ \fp2 -> ewmc n1 fp1 fp2
  return v2

main :: IO ()
main = flip evalStateT (0, Nothing) $ do

  startWatch "Arg Parsing"
  [nData, nc] <- liftIO getArgs
  nCap        <- liftIO getNumCapabilities
  let
    numData = read nData :: Int
    numChunk = case read nc :: Int of
      0 -> nCap
      n -> n
    sizeChunk = numData `div` numChunk
  logging $ "numData = " ++ show numData
  logging $ "numChunk = " ++ show numChunk
  logging $ "nCap = " ++ show nCap
  stopWatch

  startWatch "Non-Strict Initialization"
  let
    v1 = mkRandomVec 1 numData
    v2 = mkRandomVec 2 numData
    l1 = vecToList v1
    l2 = vecToList v2
  stopWatch

  startWatch "Strict Initialization"
  v1' <- liftIO $ evaluate . force $ v1
  v2' <- liftIO $ evaluate . force $ v2
  l1' <- liftIO $ evaluate . force $ l1
  l2' <- liftIO $ evaluate . force $ l2
  stopWatch

  startWatch "computation Vec"
  let
    res = chunkVec sizeChunk (ewmult v1' v2') `using` parListChunk sizeChunk rdeepseq
  res' <- liftIO $ evaluate . force $ vecConcat res
  stopWatch

  startWatch "computation List"
  let
    res2 = ewmultList l1' l2' `using` parListChunk sizeChunk rdeepseq
  res2' <- liftIO $ evaluate . force $ res2
  stopWatch

  startWatch "Getting the last element Vec"
  let x = last $ vecToList res'
  stopWatch

  startWatch "Getting the last element List"
  let y = last res2'
  stopWatch

  startWatch "Printing"
  logging $ "last element of the vector is " ++ show x
  logging $ "last element of the list is   " ++ show y
  stopWatch
