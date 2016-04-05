module FibonacciFp where

--    0  1  1  2  3  5  8 13 21 34 ..
-- +  1  1  2  3  5  8 13 21 34 55 ..
-- =  1  2  3  5  8 13 21 34 55 89 ..

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs !! n

fib' :: Int -> Integer
fib' n = (fib n) `mod` (10^8 + 7)

main :: IO ()
main = do
  print $ take 100 fibs
  mapM_ (putStrLn . show . fib') [0,1,5,10,100]
