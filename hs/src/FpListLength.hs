module FpListLength where

len :: [a] -> Int
len = foldr (\_ -> (+) 1) 0

main :: IO ()
main = do
  putStrLn "length..."

  let lst = [1,2,3,4,5,6]
      len' = len lst

  print len'
  