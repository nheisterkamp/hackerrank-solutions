-- | Main entry point to the application.
module FpFilterArray where

f1 :: Int -> [Int] -> [Int]
f1 n [] = []
f1 n (x:arr)
  | x < n = x : f1 n arr
  | otherwise = f1 n arr

--f2 :: Int -> [Int] -> [Int]
--f2 n [] = []
--f2 n (x:arr) = [x | x < n] ++ f2 n arr

-- foldr :: (a -> b -> b) -> b -> [a] -> b
f3 :: Int -> [Int] -> [Int]
f3 n = foldr (\ x -> (++) [x | x < n]) []

-- foldl :: (b -> a -> b) -> b -> [a] -> b
f4 :: Int -> [Int] -> [Int]
f4 n arr = arr

main :: IO ()
main = do
    let n = 3
        arr = [3, 10, 9, 8, 2, 7, 5, 1, 3, 0]
        f' = f4 n arr

    print f'