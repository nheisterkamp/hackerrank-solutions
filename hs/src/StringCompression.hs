module Main where

compress :: String -> String
compress s
  | null s = ""
  | l == 1 = a : compress (tail s)
  | otherwise = a : (show l) ++ compress (drop l s)
  where a = head s
        r = takeWhile (== a) s
        l = length r

main :: IO ()
main = do
  print $ compress "abcaaabbb"
  print $ compress "abcd"
  print $ compress "aaabaaaaccaaaaba"
  return ()
