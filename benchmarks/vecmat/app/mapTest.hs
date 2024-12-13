import VecMat

main :: IO ()
main = do
  let a = vecFromList [0::Double, 1, -1, 2, 3]
      res = vectorMapR Sin a
      res2 = vectorMapR Sign a

  print res
  print res2
