module Main where

matrix :: [[Int]]
matrix = [
    [ 1,  2,  3,  4],
    [ 5,  6,  7,  8],
    [ 9, 10, 11, 12],
    [13, 14, 15, 16]]

slice :: Int -> Int -> [Int] -> [Int]
slice start count l = take count $ drop start l

rotate :: Int -> Int -> [[Int]] -> [[Int]]
rotate m n a = [
        [a!!0!!1, a!!0!!2, a!!0!!3, a!!1!!3],
        [a!!0!!0, a!!1!!2, a!!2!!2, a!!2!!3],
        [a!!1!!0, a!!1!!1, a!!2!!1, a!!3!!3],
        [a!!2!!0, a!!3!!0, a!!3!!1, a!!3!!2]
    ]

rotate' m n a = [
        slice 1 3 (a!!0) ++ [(a!!1!!3)]
    ]




printM = mapM_ (print)

main :: IO ()
main = do
    -- let mat = fromLists lists
    -- putStr $ dispf 0 mat

    putStrLn "Original"
    printM matrix

    putStrLn "\nRotated"
    printM $ rotate 4 4 matrix

    putStrLn "\nRotated"
    printM $ rotate' 4 4 matrix

    putStrLn "\nTest"
    print $ matrix !! 1 !! 2
