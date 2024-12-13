module ParConduit.Computation where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Data.Vector.Storable qualified as SV
import Foreign (Ptr)
import GHC.Compact (compact, compactAdd, getCompact)
import Numeric.LinearAlgebra qualified as LA
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (Typeable, evaluateDeep)
import UnliftIO.Concurrent (threadDelay)


data Model
  = Sleep
  | Ponder
  | Reduce
  | ReduceCompact
  | NormVector
  | NormFFIUnsafe
  | NormFFISafe
  | NormBlas
  | DotFFIUnsafe
  | DotFFISafe
  | DotBlas
  | DotFFIUnsafe2
  | DotFFISafe2
  | DotBlas2
  deriving (Eq, Read, Show)


type ComputationProperty m =
  ( Computation m
  , Typeable m
  , Typeable (Load m)
  , Typeable (Input m)
  , Typeable (Output m)
  , NFData (Input m)
  , NFData (Output m)
  , NFData (Load m)
  , Show (Output m)
  , Num (Output m)
  )


withModel :: Model -> (forall m. ComputationProperty m => Proxy m -> r) -> r
withModel Sleep f = f (Proxy @'Sleep)
withModel Ponder f = f (Proxy @'Ponder)
withModel Reduce f = f (Proxy @'Reduce)
withModel ReduceCompact f = f (Proxy @'ReduceCompact)
withModel NormVector f = f (Proxy @'NormVector)
withModel NormFFIUnsafe f = f (Proxy @'NormFFIUnsafe)
withModel NormFFISafe f = f (Proxy @'NormFFISafe)
withModel NormBlas f = f (Proxy @'NormBlas)
withModel DotFFIUnsafe f = f (Proxy @'DotFFIUnsafe)
withModel DotFFISafe f = f (Proxy @'DotFFISafe)
withModel DotBlas f = f (Proxy @'DotBlas)
withModel DotFFIUnsafe2 f = f (Proxy @'DotFFIUnsafe2)
withModel DotFFISafe2 f = f (Proxy @'DotFFISafe2)
withModel DotBlas2 f = f (Proxy @'DotBlas2)


-- The computation loads are desgined to be roughly the computation time
-- 1000 ~ 1ms
-- 1000000 ~ 1s

class Computation (a :: Model) where
  type Load a
  type Input a
  type Output a
  prepareData :: MonadIO m => Int -> m [Input a]
  prepareLoad :: MonadIO m => Int -> m (Load a)
  compute :: Load a -> Input a -> Output a
  computeM :: MonadIO m => Load a -> Input a -> m (Output a)


instance Computation 'Sleep where
  type Load 'Sleep = Int
  type Input 'Sleep = Int
  type Output 'Sleep = Int
  prepareData numData = return [0 .. (numData - 1)]
  prepareLoad = return
  compute time idx = unsafePerformIO $ computeM @'Sleep time idx
  computeM time idx = do
    threadDelay time
    return idx


instance Computation 'Ponder where
  type Load 'Ponder = Int
  type Input 'Ponder = Double
  type Output 'Ponder = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad = return
  compute 0 !b = b
  compute a !b = compute @'Ponder (a - 1) (applyNTimes 100 (f a) b)
    where
      -- semantically f k x = x but very slow
      f k x = (fromIntegral (k + floor x + 1) * x) / fromIntegral (k + 1 + floor x)
  computeM time idx = evaluateDeep $ compute @'Ponder time idx


applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n g !x = g (applyNTimes (n - 1) g x)


instance Computation 'Reduce where
  type Load 'Reduce = [Double]
  type Input 'Reduce = Double
  type Output 'Reduce = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad num = return $ fmap fromIntegral [0 .. (50 * num - 1)]
  compute load a = (a * avg) / avg
    where
      avg = sum load / len
      len = fromIntegral $ length load
  computeM time idx = return $ compute @'Reduce time idx


instance Computation 'ReduceCompact where
  type Load 'ReduceCompact = [Double]
  type Input 'ReduceCompact = Double
  type Output 'ReduceCompact = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad num = do
    region <- liftIO $ compact ()
    let load = fmap fromIntegral [0 .. (50 * num - 1)]
    getCompact <$> liftIO (compactAdd region load)
  compute = compute @'Reduce
  computeM = computeM @'Reduce


instance Computation 'NormVector where
  type Load 'NormVector = Int
  type Input 'NormVector = Int
  type Output 'NormVector = Double
  prepareData numData = return [0 .. (numData - 1)]
  prepareLoad = return
  compute size seed = fromIntegral seed * nm / nm
    where
      nm = LA.norm_2 $ LA.randomVector seed LA.Gaussian (50 * size)
  computeM size seed = return $ compute @'NormVector size seed


instance Computation 'NormFFIUnsafe where
  type Load 'NormFFIUnsafe = SV.Vector Double
  type Input 'NormFFIUnsafe = Double
  type Output 'NormFFIUnsafe = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad num = do
    return $ SV.generate (1000 * num) $ \i -> fromIntegral i
  compute load a = (a * nm) / nm
    where
      nm = normVectorU load
  computeM load a = return $ compute @'NormFFIUnsafe load a


instance Computation 'NormFFISafe where
  type Load 'NormFFISafe = SV.Vector Double
  type Input 'NormFFISafe = Double
  type Output 'NormFFISafe = Double
  prepareData = prepareData @'NormFFIUnsafe
  prepareLoad = prepareLoad @'NormFFIUnsafe
  compute load a = (a * nm) / nm
    where
      nm = normVectorS load
  computeM load a = return $ compute @'NormFFISafe load a


instance Computation 'NormBlas where
  type Load 'NormBlas = SV.Vector Double
  type Input 'NormBlas = Double
  type Output 'NormBlas = Double
  prepareData = prepareData @'NormFFIUnsafe
  prepareLoad = prepareLoad @'NormFFIUnsafe
  compute load a = (a * nm) / nm
    where
      nm = LA.norm_2 load
  computeM load a = return $ compute @'NormBlas load a


foreign import ccall unsafe "norm" c_norm_u :: Int -> Ptr Double -> Double
foreign import ccall safe "norm" c_norm_s :: Int -> Ptr Double -> Double


normVectorU :: SV.Vector Double -> Double
normVectorU vec = unsafePerformIO $ SV.unsafeWith vec $ \ptr -> do
  return $ c_norm_u size ptr
  where
    size = SV.length vec


normVectorS :: SV.Vector Double -> Double
normVectorS vec = unsafePerformIO $ SV.unsafeWith vec $ \ptr -> do
  return $ c_norm_s size ptr
  where
    size = SV.length vec


instance Computation 'DotFFIUnsafe where
  type Load 'DotFFIUnsafe = (SV.Vector Double, SV.Vector Double)
  type Input 'DotFFIUnsafe = Double
  type Output 'DotFFIUnsafe = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad num = do
    let v1 = SV.generate (1000 * num) $ \i -> fromIntegral i
        v2 = SV.generate (1000 * num) $ \i -> fromIntegral (i + 1)
    return (v1, v2)
  compute (v1, v2) a = (a * nm) / nm
    where
      nm = dotVectorU v1 v2
  computeM load a = return $ compute @'DotFFIUnsafe load a


instance Computation 'DotFFISafe where
  type Load 'DotFFISafe = (SV.Vector Double, SV.Vector Double)
  type Input 'DotFFISafe = Double
  type Output 'DotFFISafe = Double
  prepareData = prepareData @'DotFFIUnsafe
  prepareLoad = prepareLoad @'DotFFIUnsafe
  compute (v1, v2) a = (a * nm) / nm
    where
      nm = dotVectorU v1 v2
  computeM load a = return $ compute @'DotFFISafe load a


instance Computation 'DotBlas where
  type Load 'DotBlas = (SV.Vector Double, SV.Vector Double)
  type Input 'DotBlas = Double
  type Output 'DotBlas = Double
  prepareData = prepareData @'DotFFIUnsafe
  prepareLoad = prepareLoad @'DotFFIUnsafe
  compute (v1, v2) a = (a * nm) / nm
    where
      nm = LA.dot v1 v2
  computeM load a = return $ compute @'DotBlas load a


instance Computation 'DotFFIUnsafe2 where
  type Load 'DotFFIUnsafe2 = Int
  type Input 'DotFFIUnsafe2 = Double
  type Output 'DotFFIUnsafe2 = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad = return
  compute num a = (a * nm) / nm
    where
      v1 = SV.generate (200 * num) $ \i -> fromIntegral i
      v2 = SV.generate (200 * num) $ \i -> fromIntegral (i + 1)
      nm = dotVectorU v1 v2
  computeM load a = return $ compute @'DotFFIUnsafe2 load a


instance Computation 'DotFFISafe2 where
  type Load 'DotFFISafe2 = Int
  type Input 'DotFFISafe2 = Double
  type Output 'DotFFISafe2 = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad = return
  compute num a = (a * nm) / nm
    where
      v1 = SV.generate (200 * num) $ \i -> fromIntegral i
      v2 = SV.generate (200 * num) $ \i -> fromIntegral (i + 1)
      nm = dotVectorS v1 v2
  computeM load a = return $ compute @'DotFFISafe2 load a


instance Computation 'DotBlas2 where
  type Load 'DotBlas2 = Int
  type Input 'DotBlas2 = Double
  type Output 'DotBlas2 = Double
  prepareData numData = return $ fmap fromIntegral [0 .. (numData - 1)]
  prepareLoad = return
  compute num a = (a * nm) / nm
    where
      v1 = SV.generate (200 * num) $ \i -> fromIntegral i
      v2 = SV.generate (200 * num) $ \i -> fromIntegral (i + 1)
      nm = LA.dot v1 v2
  computeM load a = return $ compute @'DotBlas2 load a



foreign import ccall unsafe "dot" c_dot_u :: Int -> Ptr Double -> Ptr Double -> Double
foreign import ccall safe "dot" c_dot_s :: Int -> Ptr Double -> Ptr Double -> Double


dotVectorU :: SV.Vector Double -> SV.Vector Double -> Double
dotVectorU v1 v2 = unsafePerformIO $ SV.unsafeWith v1 $ \ptr1 ->
  SV.unsafeWith v2 $ \ptr2 -> do
    return $ c_dot_u size ptr1 ptr2
  where
    size = SV.length v1


dotVectorS :: SV.Vector Double -> SV.Vector Double -> Double
dotVectorS v1 v2 = unsafePerformIO $ SV.unsafeWith v1 $ \ptr1 ->
  SV.unsafeWith v2 $ \ptr2 -> do
    return $ c_dot_s size ptr1 ptr2
  where
    size = SV.length v1
