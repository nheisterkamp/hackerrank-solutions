module Main where

import Data.List

-- 1
-- 1 1
-- 1 2 1
-- 1 3 3 1
-- 1 4 6 4 1
-- ....
row :: Int -> [Int]
row 1 = [1]
row n = [1] ++ zipWith (+) row' (tail row') ++ [1]
  where row' = row (n - 1)

showRow :: [Int] -> String
showRow r = concat
  $ intersperse " "
  $ map show r

triangle :: Int -> IO ()
triangle n =
  mapM_ (putStrLn . showRow)
  $ map row [1..n]


main :: IO ()
main = do
  let n = 10
  triangle n
