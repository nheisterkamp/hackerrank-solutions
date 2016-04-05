module Main where

p, q :: String
p = "hacker"
q = "ranker"

r :: String -> String -> String
r (a:as) (b:bs) = a : b : r as bs
r _ _ = ""

main :: IO ()
main = do
  print p
  print q
  print $ r p q

  print $ concat $ zipWith (\a b -> [a] ++ [b]) p q


  -- for the solution
  p <- getLine
  q <- getLine
  putStrLn $ r p q
