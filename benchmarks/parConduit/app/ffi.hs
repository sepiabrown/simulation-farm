module Main where

import Control.Concurrent
import Foreign.C
import qualified Data.Vector.Storable as SV
import ParConduit.Computation (dotVectorS, dotVectorU)
import Control.Monad (forM)
import UnliftIO.Async (async, wait)

main = do
  ostid <- get_ostid
  putStrLn ("haskell thread ostid = " ++ show ostid)
  ws <- forM [1..7] $ \i -> async (work i)
  forM ws wait

foreign import ccall safe get_ostid :: IO CUInt

work :: CUInt -> IO ()
work n = do
  ht <- myThreadId
  putStrLn ("n = " ++ show n)
  putStrLn (show ht ++ " starts")
  ostid <- get_ostid
  putStrLn ("ostid = " ++ show ostid)
  let v1 = SV.generate @Double 1000000 $ \i -> 1.0 / (fromIntegral n - fromIntegral n + fromIntegral i + 1)
  let r = dotVectorU v1 v1
  putStrLn ("result = " ++ show r)
  putStrLn (show ht ++ " ends")

foreign import ccall safe slow :: CUInt -> IO ()
