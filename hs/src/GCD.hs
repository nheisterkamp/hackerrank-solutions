module Main where

gcd' :: Int -> Int -> Int
gcd' x y
  | x == y = x
  | x > y = gcd (x-y) y
  | otherwise = gcd x (y-x)

main :: IO ()
main = do
  print $ gcd' 1 5
  print $ gcd' 10 100
  print $ gcd' 22 131
