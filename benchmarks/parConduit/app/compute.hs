module Main where

import Control.Concurrent (getNumCapabilities)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (:~:) (Refl))
import Data.Typeable (eqT)
import GHC.Utils.Misc (chunkList)
import Logging (logging, startWatch, stopWatch)
import ParConduit.Computation (Computation (..), withModel)
import ParConduit.Parallelizer (Parallel (..), ParallelProperty, Parallelizer (..), withParallelizer)
import System.Environment (getArgs)
import UnliftIO (BufferMode (NoBuffering), evaluateDeep, hSetBuffering, stdout)


main :: IO ()
main = flip evalStateT (0, Nothing) $ do
  hSetBuffering stdout NoBuffering

  startWatch "Arg Parsing"
  (comModel, numData :: Int, sizeChunk :: Int, workLoad :: Int, parallelizer) <-
    liftIO $
      getArgs >>= \case
        [comModel, nData, sChunk, nLoad, parallelizer] ->
          return (comModel, read nData, read sChunk, read nLoad, parallelizer)
        _ -> error "Expected arguments are computation, numData, sizeChunk, workLoad, and parallelizer"

  numCap <- liftIO getNumCapabilities
  stopWatch
  logging $ "computationModel = " ++ comModel
  logging $ "numData = " ++ show numData
  logging $ "sizeChunk = " ++ show sizeChunk
  logging $ "workLoad = " ++ show workLoad
  logging $ "parallelizer = " ++ parallelizer
  logging $ "numCapability = " ++ show numCap

  withModel (read comModel) $ \(_ :: Proxy cm) -> do
    startWatch "Non-Strict Initialization"
    xs <- prepareData @cm numData
    let numChunk = length $ chunkList sizeChunk xs
    stopWatch
    logging $ "numChunk = " ++ show numChunk

    withParallelizer @(Input cm) @(Output cm) (read parallelizer) $ \(_ :: Proxy p) -> do
      startWatch "Strict Initialization"
      xs' <- evaluateDeep xs
      load <- evaluateDeep =<< prepareLoad @cm workLoad
      stopWatch

      let computation = parallel @p @(Output cm) numCap sizeChunk xs' (fn @cm @p load)

      startWatch $ comModel ++ " parallelization using " ++ parallelizer
      ys <- liftIO $ evaluateDeep =<< computation
      stopWatch

      startWatch "Printing"
      logging $ "The sum of data is " ++ show (sum ys)
      stopWatch


fn :: forall cm p. (Computation cm, ParallelProperty p) => Load cm -> Fn p (Input cm) (Output cm)
fn load = case eqT @p @'ConduitMVar of
  Just Refl -> computeM @cm load
  Nothing -> case eqT @p @'ConduitTMVar of
    Just Refl -> computeM @cm load
    Nothing -> case eqT @p @'ConduitQSem of
      Just Refl -> computeM @cm load
      Nothing -> case eqT @p @'ConduitCapPool of
        Just Refl -> computeM @cm load
        Nothing -> case eqT @p @'ConduitCapPool2 of
          Just Refl -> computeM @cm load
          Nothing -> case eqT @p @'Concurrently of
            Just Refl -> computeM @cm load
            Nothing -> case eqT @p @'ConcurrentlyOn of
              Just Refl -> computeM @cm load
              Nothing -> case eqT @p @'Async of
                Just Refl -> computeM @cm load
                Nothing -> case eqT @p @'AsyncBound of
                  Just Refl -> computeM @cm load
                  Nothing -> case eqT @p @'ParList of
                    Just Refl -> compute @cm load
                    Nothing -> case eqT @p @'ParMap of
                      Just Refl -> compute @cm load
                      Nothing -> error "Unkown parallelizer"
