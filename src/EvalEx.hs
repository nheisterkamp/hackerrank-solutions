module EvalEx where

terms = 10

-- 1 + x + x^2/2! + x^3/3! + x^4/4! + ...
solve :: Double -> Double
solve x = sum [x ^ n / fromIntegral (product [1..n]) | n <- [0..terms-1]]

main :: IO ()
main = do
  putStrLn "e^x expansion"

  mapM_ (print . solve) [
      20.0000
    , 5.0000
    , 0.5000
    , -0.5000
    ]
