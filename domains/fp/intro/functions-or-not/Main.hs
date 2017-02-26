import Data.List (nub, sort)

isFunction :: [(Int, Int)] -> Bool
isFunction a = all allEq b
  where xs = nub $ map fst a
        ys x = filter (\(z,_) -> z == x) a
        b = map ys xs
        allEq xs = and $ map (== head xs) (tail xs)

parse :: [String] -> [[(Int, Int)]]
parse [] = []
parse a = c : (parse (drop (n+1) a))
  where n = (read . head) a :: Int
        c = sort c'
        c' = map w (take n (tail a))
        w s = (w' s!!0, w' s!!1)
        w' s = map read (words s) :: [Int]

main :: IO ()
main = do
  raw <- getContents

  let cases = parse ((tail . lines) raw)

  mapM_ (putStrLn . yesNo . isFunction) cases
    where yesNo x
            | x == True = "YES"
            | x == False = "NO"
