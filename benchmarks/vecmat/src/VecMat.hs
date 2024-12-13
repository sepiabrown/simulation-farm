{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}


module VecMat where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (when)
import Foreign (Storable(sizeOf))
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.Ptr (Ptr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes, unsafeForeignPtrToPtr, ForeignPtr (ForeignPtr))
import GHC.IO (unsafePerformIO)
import Foreign.Marshal.Array
    ( withArray, copyArray, peekArray, advancePtr )
import Prelude hiding ((<>), elem)
import Data.List (unfoldr)
import GHC.Ptr (Ptr(..))

data Vec a = Vec
  {
    dim  :: {-# UNPACK #-} !Int
  , elem :: {-# UNPACK #-} !(ForeignPtr a)
  }

instance NFData (Vec a) where
  rnf (Vec {}) = ()

data Mat a = Mat
  { nrows :: {-# UNPACK #-} !Int
  , ncols :: {-# UNPACK #-} !Int
  , elems :: {-# UNPACK #-} !(ForeignPtr a)
  }

fromei :: Enum a => a -> Int
fromei x = fromEnum x :: Int

data FunCodeV = Sin
              | Cos
              | Tan
              | Abs
              | ASin
              | ACos
              | ATan
              | Sinh
              | Cosh
              | Tanh
              | ASinh
              | ACosh
              | ATanh
              | Exp
              | Log
              | Sign
              | Sqrt
              deriving Enum

instance NFData (Mat a) where
  rnf (Mat {}) = ()

instance Show (Vec Double) where
  show v = show $ vecToList v

instance Show (Mat Double) where
  show m = show $ chunk (ncols m) $ matToList m

size :: Mat t -> (Int, Int)
size m = (nrows m, ncols m)
{-# INLINE size #-}

matFromVec :: Int -> Int -> Vec a -> Mat a
matFromVec r c v
    | r * c == dim v = m
    | otherwise = error $ "can't reshape vector dim = " ++ show (dim v) ++ " to matrix " ++ showSize m
  where
    m = Mat { nrows = r, ncols = c, elems = elem v }

vecFromList :: Storable a => [a] -> Vec a
vecFromList xs = unsafePerformIO $ do
  let n = length xs
  res <- createVec n
  _ <- withArray xs $ \ptr -> unsafeVecWith res $ \mptr -> copyArray mptr ptr n
  return res

matFromList :: Storable a => Int -> Int -> [a] -> Mat a
matFromList nr nc xs
  | nr * nc > length xs = error "size of matrix is bigger than the number of elements in list."
  | otherwise = unsafePerformIO $ do
      res <- createMat nr nc
      let sz = nr * nc
      _ <- withArray xs $ \ptr -> unsafeMatWith res $ \mptr -> copyArray mptr ptr sz
      return res

vecToList :: Vec Double -> [Double]
vecToList m = unsafePerformIO $ peekArray (dim m) (unsafeForeignPtrToPtr (elem m) :: Ptr Double)

matToList :: Mat Double -> [Double]
matToList m = unsafePerformIO $ peekArray n (unsafeForeignPtrToPtr (elems m) :: Ptr Double)
  where n = nrows m * ncols m

showSize :: Mat t -> String
showSize = showDim . size

showDim :: (Show a, Show a1) => (a1, a) -> String
showDim (r,c) = "(" ++ show r ++ "x" ++ show c ++ ")"

createVec :: Storable a => Int -> IO (Vec a)
createVec n = do
    when (n < 0) $ error ("trying to createVec of negative dim: " ++ show n)
    fp <- doMalloc undefined
    return $ Vec n fp
  where
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = mallocPlainForeignPtrBytes (n * sizeOf dummy)

createMat :: Storable a => Int -> Int -> IO (Mat a)
createMat r c = do
    p <- createVec (r * c)
    return (matFromVec r c p)

foreign import ccall unsafe "random_vector" c_random_vector :: Int -> Int -> Int -> Ptr Double -> IO Int
foreign import ccall safe "random_vector" c_random_vectorSafe :: Int -> Int -> Int -> Ptr Double -> IO Int

mkRandomVec :: Int -> Int -> Vec Double
mkRandomVec seed n = unsafePerformIO $ do
  res <- createVec n
  _ <- unsafeVecWith res $ \ptr -> c_random_vector seed 1 n ptr -- 1 is for Gaussian, 0 for uniform
  return res

mkRandomVecSafe :: Int -> Int -> Vec Double
mkRandomVecSafe seed n = unsafePerformIO $ do
  res <- createVec n
  _ <- unsafeVecWith res $ \ptr -> c_random_vectorSafe seed 1 n ptr
  return res

mkRandomMat :: Int -> Int -> Int -> Mat Double
mkRandomMat seed nr nc = unsafePerformIO $ do
  res <- createMat nr nc
  let sz = nr * nc
  _ <- unsafeMatWith res $ \ptr -> c_random_vector seed 1 sz ptr
  return res

mkRandomMatSafe :: Int -> Int -> Int -> Mat Double
mkRandomMatSafe seed nr nc = unsafePerformIO $ do
  res <- createMat nr nc
  let sz = nr * nc
  _ <- unsafeMatWith res $ \ptr -> c_random_vectorSafe seed 1 sz ptr
  return res

unsafeMatWith :: Mat a -> (Ptr a -> IO r) -> IO r
{-# INLINE unsafeMatWith #-}
unsafeMatWith (Mat _  _ fp) = withForeignPtr fp

unsafeVecWith :: Vec a -> (Ptr a -> IO r) -> IO r
{-# INLINE unsafeVecWith #-}
unsafeVecWith (Vec _ fp) = withForeignPtr fp

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

chunkVec :: Storable a => Int -> Vec a -> [Vec a]
chunkVec n = unfoldr go
  where go v | isVecNull v = Nothing
             | otherwise = Just (splitVecAt n v)

isVecNull :: Vec a -> Bool
isVecNull v = dim v == 0

splitVecAt :: Storable a => Int -> Vec a -> (Vec a, Vec a)
splitVecAt n v = (unsafeVecSlice 0 m v, unsafeVecSlice m (delayInline max 0 (len - n')) v)
  where
    m = delayInline min n' len
    n' = max n 0
    len = dim v

delayInline :: (a -> b) -> a -> b
{-# INLINE [0] delayInline #-}
delayInline f = f

unsafeVecSlice :: Storable a => Int -> Int -> Vec a -> Vec a
unsafeVecSlice = basicUnsafeVecSlice

basicUnsafeVecSlice :: Storable a => Int -> Int -> Vec a -> Vec a
basicUnsafeVecSlice i n (Vec _ fp) = Vec n (updPtr (`advancePtr` i) fp)

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of Ptr q -> ForeignPtr q c

foreign import ccall unsafe "mapR" c_vectorMapR :: Int -> Int -> Ptr Double -> Int -> Ptr Double -> IO Int
foreign import ccall safe "mapR" c_vectorMapRSafe :: Int -> Int -> Ptr Double -> Int -> Ptr Double -> IO Int

vectorMapR :: FunCodeV -> Vec Double -> Vec Double
vectorMapR code v = unsafePerformIO $ do
  let n = dim v
  r <- createVec n
  _ <- unsafeVecWith v $ \fp1 ->
         unsafeVecWith r $ \fp -> c_vectorMapR (fromei code) n fp1 n fp
  return r

vectorMapRSafe :: FunCodeV -> Vec Double -> Vec Double
vectorMapRSafe code v = unsafePerformIO $ do
  let n = dim v
  r <- createVec n
  _ <- unsafeVecWith v $ \fp1 ->
         unsafeVecWith r $ \fp -> c_vectorMapRSafe (fromei code) n fp1 n fp
  return r

reshape :: Int -> Vec a -> Mat a
reshape 0 v = matFromVec 0 0 v
reshape c v = matFromVec (dim v `div` c) c v

asColumn :: Vec a -> Mat a
asColumn = reshape 1

flatten :: Mat a -> Vec a
flatten Mat {..} = Vec (nrows * ncols) elems

foreign import ccall unsafe "sumR_int" c_sumVec :: Int -> Ptr Double -> IO Double
foreign import ccall safe "sumR_int" c_sumVecSafe :: Int -> Ptr Double -> IO Double

sumVec :: Vec Double -> Double
sumVec v = unsafePerformIO $
  unsafeVecWith v $ \fp -> c_sumVec (dim v) fp

sumVecSafe :: Vec Double -> Double
sumVecSafe v = unsafePerformIO $
  unsafeVecWith v $ \fp -> c_sumVecSafe (dim v) fp
