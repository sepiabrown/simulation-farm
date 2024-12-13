import VecMat

main :: IO ()
main = do
  let
    v = vecFromList [1::Double,2,3,4,5,6]
    (v1, v2) = splitVecAt 2 v
  print v1
  print v2
  let vs = chunkVec 4 v
      vs2 = chunkVec 8 v
  print vs
  print vs2
