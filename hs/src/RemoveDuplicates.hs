solve [] a = a
solve (x:xs) a
  | x `elem` a = solve xs a
  | otherwise  = solve xs (a ++ [x])

main = do
  let input = "ccbabacc"
  putStrLn $ solve input []
