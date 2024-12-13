import MyFFI
import VecMat

main :: IO ()
main = do
  let
    v = vecFromList [1,2,3]
    m = matFromList 2 3 [1,2,3,4,5,6]
    res = mXv m v
  print res
