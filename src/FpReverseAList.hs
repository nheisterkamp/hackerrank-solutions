module FpReverseAList where

rev :: [Int] -> [Int]
rev = foldl (flip (:)) []
  
main :: IO ()
main = do
  putStrLn "rev.."
  
  let arr = [19, 22, 3, 28, 26, 17, 18, 4, 28, 0]
      rev' = rev arr

  print arr
  print rev'