module FpFilterPositionsInAList where

f :: [Int] -> [Int]
f lst = [lst!!x | x <- [1,3..length lst-1]]

main :: IO ()
main = do
  putStrLn "odd.."

  let lst = [2,5,3,4,6,7,9]
      f' = f lst

  print lst
  print f'