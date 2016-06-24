solve x n = solve' x ys 0
  where ys = takeWhile (<= x) (map (\x' -> x'^n) [1..x])

solve' _ [] _ = 0
solve' x (y:ys) r
  | r + y >  x = 0
  | r + y == x = 1
  | otherwise = solve' x ys (r+y) + solve' x ys r

main :: IO ()
main = getLine >>= \x -> getLine >>= \n -> print $ 
  solve (read x :: Int) (read n :: Int)
