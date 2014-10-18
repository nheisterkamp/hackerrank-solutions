module FpSumOfOddElements where

f :: [Int] -> Int
f = foldr (\x -> (+) (x * mod x 2)) 0
f1 arr = sum [x | x <- arr, x `mod` 2 == 1]
f2 = foldr (\ x -> (+) (if x `mod` 2 == 0 then 0 else x)) 0


main :: IO ()
main = do
  let arr = [2,3,4,6,5,7,8,0,1]
      f' = f arr

  print arr
  print f'