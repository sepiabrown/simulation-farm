module MyFFI where

import Control.Monad (when)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)

import VecMat

type MatD = Mat Double
type VecD = Vec Double

foreign import ccall safe "mmult" c_mmultSafe :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> Int -> Int -> IO ()
foreign import ccall unsafe "mmult" c_mmult :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> Int -> Int -> IO ()

mXmSafe :: MatD -> MatD -> MatD
mXmSafe m1 m2 = {-# SCC "mmultSafe" #-}unsafePerformIO $ do
  let nr1 = nrows m1
      nc1 = ncols m1
      nr2 = nrows m2
      nc2 = ncols m2
  when (nc1 /= nr2) $ error "inconsistent dimensions in matrix multiplication"
  res <- createMat nr1 nc2
  _ <- unsafeMatWith m1 $ \fp1 ->
         unsafeMatWith m2 $ \fp2 ->
           unsafeMatWith res $ \fp -> c_mmultSafe fp1 fp2 fp nr1 nc1 nc2
  return res

mXm :: MatD -> MatD -> MatD
mXm m1 m2 = {-# SCC "mmultUnsafe" #-}unsafePerformIO $ do
  let nr1 = nrows m1
      nc1 = ncols m1
      nr2 = nrows m2
      nc2 = ncols m2
  when (nc1 /= nr2) $ error "inconsistent dimensions in matrix multiplication"
  res <- createMat nr1 nc2
  _ <- unsafeMatWith m1 $ \fp1 ->
         unsafeMatWith m2 $ \fp2 ->
           unsafeMatWith res $ \fp -> c_mmult fp1 fp2 fp nr1 nc1 nc2
  return res

mXv :: MatD -> VecD -> VecD
mXv m v = flatten $ mXm m v'
  where
    v' = asColumn v

mXvSafe :: MatD -> VecD -> VecD
mXvSafe m v = flatten $ mXmSafe m v'
  where
    v' = asColumn v
