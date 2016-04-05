module Main where

import Data.List (sort)

main = do
  total <- getLine
  contents <- getContents
  let steps = map (read :: String -> Int) (lines contents)
  --let steps = []

  let result = solve steps
  mapM_ print result

median xs = sort xs !! ((l' `div` 2) - 1)
  where l = length xs
        l' = l + (l `mod` 2)

last' a
  | null a = []
  | otherwise = last a

solveStep :: [[Int]] -> Int -> [Int]
solveStep stack step
  | step < 0 = stack !! (length stack + step)
  | otherwise = lst ++ [step]
  where lst = last' stack

solve steps = map median sol
  where sol = solve' steps

solve' steps = foldl (\a b -> a ++ [(solveStep a b)]) [] steps

