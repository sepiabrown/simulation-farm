module Main where

import MyBlas
import VecMat

main :: IO ()
main = do
  let
    m1 = matFromList 2 3 [1::Double,2,3,4,5,6]
    m2 = matFromList 3 2 [2::Double,3,1,6,5,4]
    res = mXm m1 m2
  print   "result should be          [[19.0,27.0],[43.0,66.0]]"
  print $ "result from blas dgemm is " ++ show res
