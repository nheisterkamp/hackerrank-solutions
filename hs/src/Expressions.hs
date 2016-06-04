-- import Debug.Trace (trace)

merge (a:as) [] = a
merge (a:as) (b:bs) = a ++ [b] ++ merge as bs

solve i = merge (map show i) (solve' (head i) [] (tail i))

-- solve' c o i | trace ("solve' " ++ show c ++ " " ++ show o) False = undefined

solve' c o []
  | c `mod` 101 == 0 = o
  | otherwise = []

solve' c o (x:xs)
  | u /= "" && u /= o = u
  | v /= "" && v /= o = v
  | w /= "" && w /= o = w
  | otherwise = []
  where u = solve' (c * x) (o ++ "*") xs
        v = solve' (c + x) (o ++ "+") xs
        w = solve' (c - x) (o ++ "-") xs

main = do
  n <- getLine
  input <- getLine
  -- let input = "55 3 45 33 25"
  let i = map read (words input) :: [Int]

  putStrLn (solve i)
