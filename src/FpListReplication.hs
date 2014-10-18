module FpListReplication where

f :: Int -> [Int] -> [Int]
f n = foldr ((++) . replicate n) []

main :: IO ()
main = do
  let arr = [1,2,3,4]
      n = 3
      f' = f n arr

  print f'