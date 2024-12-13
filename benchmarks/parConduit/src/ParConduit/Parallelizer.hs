{-# LANGUAGE PolyKinds #-}

module ParConduit.Parallelizer where

import Control.DeepSeq (NFData, force)
import Control.Monad (forM, (<=<))
import Control.Monad.Par (parMap, runPar)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Proxy (Proxy (..))
import GHC.Utils.Misc (chunkList)
import HUtils.Concurrent (forConcurrentlyOn)
import HUtils.Conduit.Parallel
  ( SharingVar (..)
  , mapParallelM
  , runConduitParallel
  , sinkListParallel
  , yieldParallel
  , (.|)
  )
import UnliftIO (Typeable, evaluateDeep, pooledForConcurrently)
import UnliftIO.Async (asyncBound, wait, async)


data Parallelizer
  = ConduitMVar
  | ConduitTMVar
  | ConduitQSem
  | ConduitCapPool
  | ConduitCapPool2
  | ConcurrentlyOn
  | Concurrently
  | Async
  | AsyncBound
  | ParList
  | ParMap
  deriving (Eq, Read, Show)


type ParallelProperty p =
  ( Parallel p
  , Typeable p
  )


withParallelizer
  :: (Typeable input, Typeable output)
  => Parallelizer
  -> (forall p. (ParallelProperty p, Typeable (Fn p input output)) => Proxy p -> r)
  -> r
withParallelizer ConduitMVar f = f @'ConduitMVar Proxy
withParallelizer ConduitTMVar f = f @'ConduitTMVar Proxy
withParallelizer ConduitQSem f = f @'ConduitQSem Proxy
withParallelizer ConduitCapPool f = f @'ConduitCapPool Proxy
withParallelizer ConduitCapPool2 f = f @'ConduitCapPool2 Proxy
withParallelizer Concurrently f = f @'Concurrently Proxy
withParallelizer ConcurrentlyOn f = f @'ConcurrentlyOn Proxy
withParallelizer Async f = f @'Async Proxy
withParallelizer AsyncBound f = f @'AsyncBound Proxy
withParallelizer ParList f = f @'ParList Proxy
withParallelizer ParMap f = f @'ParMap Proxy


class Parallel (p :: Parallelizer) where
  type Fn p a b
  parallel :: NFData b => Int -> Int -> [a] -> Fn p a b -> IO [b]


instance Parallel 'ConduitMVar where
  type Fn 'ConduitMVar a b = a -> IO b
  parallel numCap sizeChunk xs fn = do
    let theConduit =
          yieldParallel sizeChunk xs
            .| mapParallelM (evaluateDeep <=< fn)
            .| sinkListParallel
    runConduitParallel @'MVar numCap theConduit


instance Parallel 'ConduitTMVar where
  type Fn 'ConduitTMVar a b = a -> IO b
  parallel numCap sizeChunk xs fn = do
    let theConduit =
          yieldParallel sizeChunk xs
            .| mapParallelM (evaluateDeep <=< fn)
            .| sinkListParallel
    runConduitParallel @'TMVar numCap theConduit


instance Parallel 'ConduitQSem where
  type Fn 'ConduitQSem a b = a -> IO b
  parallel numCap sizeChunk xs fn = do
    let theConduit =
          yieldParallel sizeChunk xs
            .| mapParallelM (evaluateDeep <=< fn)
            .| sinkListParallel
    runConduitParallel @'QSem numCap theConduit


instance Parallel 'ConduitCapPool where
  type Fn 'ConduitCapPool a b = a -> IO b
  parallel numCap sizeChunk xs fn = do
    let theConduit =
          yieldParallel sizeChunk xs
            .| mapParallelM (evaluateDeep <=< fn)
            .| sinkListParallel
    runConduitParallel @'CapPool numCap theConduit


instance Parallel 'ConduitCapPool2 where
  type Fn 'ConduitCapPool2 a b = a -> IO b
  parallel numCap sizeChunk xs fn = do
    let theConduit =
          yieldParallel sizeChunk xs
            .| mapParallelM (evaluateDeep <=< fn)
            .| sinkListParallel
    runConduitParallel @'CapPool2 numCap theConduit


instance Parallel 'ConcurrentlyOn where
  type Fn 'ConcurrentlyOn a b = a -> IO b
  parallel _ sizeChunk xs fn = do
    let chunks = chunkList sizeChunk xs
    ys <- forConcurrentlyOn chunks $ \chunk -> do
      evaluateDeep =<< mapM (evaluateDeep <=< fn) chunk
    evaluateDeep $ concat ys


instance Parallel 'Concurrently where
  type Fn 'Concurrently a b = a -> IO b
  parallel _ sizeChunk xs fn = do
    let chunks = chunkList sizeChunk xs
    ys <- pooledForConcurrently chunks $ \chunk -> do
      evaluateDeep =<< mapM (evaluateDeep <=< fn) chunk
    evaluateDeep $ concat ys


instance Parallel 'Async where
  type Fn 'Async a b = a -> IO b
  parallel _ sizeChunk xs fn = do
    let chunks = chunkList sizeChunk xs
    as <- forM chunks $ \chunk -> async $ evaluateDeep =<< mapM fn chunk
    concat <$> forM as wait


instance Parallel 'AsyncBound where
  type Fn 'AsyncBound a b = a -> IO b
  parallel _ sizeChunk xs fn = do
    let chunks = chunkList sizeChunk xs
    as <- forM chunks $ \chunk -> asyncBound $ evaluateDeep =<< mapM fn chunk
    concat <$> forM as wait


instance Parallel 'ParList where
  type Fn 'ParList a b = a -> b
  parallel _ sizeChunk xs fn =
    return . force $
      (fmap (force . fn) xs `using` parListChunk sizeChunk rdeepseq)


instance Parallel 'ParMap where
  type Fn 'ParMap a b = a -> b
  parallel _ sizeChunk xs fn =
    return . force . concat . runPar $
      parMap (fmap (force . fn)) chunks
    where
      chunks = chunkList sizeChunk xs
