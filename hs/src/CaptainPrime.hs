primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
  where sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
          where (h,~(_:t)) = span (< p*p) xs

isPrime :: Integer -> Bool
isPrime n = elem n $ takeWhile (<= n) primes

isPrime' :: String -> Bool
isPrime' n = isPrime (read n :: Integer)

lefts, rights :: String -> [String]
lefts = init . lefts'
  where lefts' [] = []
        lefts' (s:t) = t : lefts' t

rights [] = []
rights s = s : rights (init s)

left, right :: String -> Bool
left s = all isPrime' (lefts s)
right s = all isPrime' (rights s)

solve s
  | elem '0' s = "DEAD"
  | otherwise = solution (p, l, r)
  where p = isPrime' s
        l = left s
        r = right s

solution (True, True, True) = "CENTRAL"
solution (True, True, False) = "LEFT"
solution (True, False, True) = "RIGHT"
solution _ = "DEAD"

main :: IO ()
main = do
  input <- getContents
  mapM_ (putStrLn . solve) $ (tail . lines) input
