{-# LANGUAGE ScopedTypeVariables #-}

module MyBlas where

import VecMat
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Control.Monad (when)

type MatD = Mat Double
type VecD = Vec Double

foreign import ccall unsafe "dgemm" dgemmc :: Int -> Int -> Int ->
                                              Ptr Double -> Ptr Double -> Ptr Double ->

                                              IO ()
foreign import ccall safe "dgemm" dgemmcSafe :: Int -> Int -> Int ->
                                                Ptr Double -> Ptr Double -> Ptr Double ->
                                                IO ()


mXm :: MatD -> MatD -> MatD
mXm m1 m2 = unsafePerformIO $ do
  let nr1 = nrows m1
      nc1 = ncols m1
      nr2 = nrows m2
      nc2 = ncols m2
      m = nr1
      n = nc2
      k = nc1
  when (nc1 /= nr2) $ error "inconsistent dimensions in matrix multiplication"
  (res :: MatD) <- createMat nr1 nc2
  _ <- unsafeMatWith m1 $ \fp1 ->
         unsafeMatWith m2 $ \fp2 ->
           unsafeMatWith res $ \fp -> dgemmc m n k fp1 fp2 fp
  return res

mXmSafe :: MatD -> MatD -> MatD
mXmSafe m1 m2 = unsafePerformIO $ do
  let nr1 = nrows m1
      nc1 = ncols m1
      nr2 = nrows m2
      nc2 = ncols m2
      m = nr1
      n = nc2
      k = nc1
  when (nc1 /= nr2) $ error "inconsistent dimensions in matrix multiplication"
  (res :: MatD) <- createMat nr1 nc2
  _ <- unsafeMatWith m1 $ \fp1 ->
         unsafeMatWith m2 $ \fp2 ->
           unsafeMatWith res $ \fp -> dgemmcSafe m n k fp1 fp2 fp
  return res

mXv :: MatD -> VecD -> VecD
mXv m v = flatten $ mXm m v'
  where
    v' = asColumn v

mXvSafe :: MatD -> VecD -> VecD
mXvSafe m v = flatten $ mXmSafe m v'
  where
    v' = asColumn v
