{-# LANGUAGE LambdaCase #-}

module Logging where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT)
import Control.Monad.State.Lazy (StateT(StateT))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

startWatch :: String -> StateT (Int, Maybe (UTCTime, String)) IO ()
startWatch title = StateT $ \(i, _) -> do
  t0 <- getCurrentTime
  putStrLn $ show i ++ ": -----------------------------------------"
  putStrLn $ show i ++ ": [" ++ title ++ "]"
  return ((), (i, Just (t0, title)))


stopWatch :: StateT (Int, Maybe (UTCTime, String)) IO ()
stopWatch = StateT $ \case
  (i, Just (t0, title)) -> do
    t1 <- getCurrentTime
    putStrLn $ show i ++ ": took " ++ show (diffUTCTime t1 t0)
    putStrLn $ show i ++ ": -----------------------------------------"
    putStrLn ""
    return ((), (i + 1, Just (t1, title)))

  (_, Nothing) -> error "A stopWatch is called before the matching startWatch"

logging :: String -> StateT (Int, Maybe (UTCTime, String)) IO ()
logging msg = StateT $ \s@(i, _) -> do
  liftIO . putStrLn $ show i ++ ": " ++ msg
  return ((), s)
