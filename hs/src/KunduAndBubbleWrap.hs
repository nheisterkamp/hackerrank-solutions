solve n m = sum [(n*m)/x | x <- [1..(n*m)]]

main :: IO ()
main = do
  nm <- getLine
  let nm' = words nm
      n = read (head nm') :: Double
      m = read (last nm') :: Double
  print $ solve n m
