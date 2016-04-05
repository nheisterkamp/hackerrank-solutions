module Main where

count, count' :: Integer -> Integer -> Integer
count n k
  | k > n           = undefined
  | k == 0          = 1
  | k > (n `div` 2) = n `count` (n-k)
  | otherwise       = n * ((n-1) `count` (k-1)) `div` k

count' n k = (count n k) `mod` 100000007

main :: IO ()
main = do
  _ <- getLine
  contents <- getContents
  mapM_ (countInput) (lines contents)

countInput input =
  print . uncurry count' . listToTuple . convertToInt . words $ input
 where
  listToTuple (x:xs:_) = (x,xs)
  convertToInt = map (read :: String -> Integer)
