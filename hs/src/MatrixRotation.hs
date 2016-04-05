module MatrixRotation where

matrix :: [[Int]]
-- matrix = [
--     [ 1,  2,  3,  4],
--     [ 5,  6,  7,  8],
--     [ 9, 10, 11, 12],
--     [13, 14, 15, 16]]
-- matrix = [
--     [ 1,  2,  3,  4],
--     [ 5,  6,  7,  8],
--     [ 9, 10, 11, 12],
--     [13, 14, 15, 16],
--     [17, 18, 19, 20],
--     [21, 22, 23, 24]]

-- matrix = [
--     [ 1,  2,  3,  4,  5,  6],
--     [ 7,  8,  9, 10, 11, 12],
--     [13, 14, 15, 16, 17, 18], 
--     [19, 20, 21, 22, 23, 24]]

matrix = [
    [ 1,  2,  3,  4,  5,  6,  7,  8],
    [ 9, 10, 11, 12, 13, 14, 15, 16], 
    [17, 18, 19, 20, 21, 22, 23, 24],
    [25, 26, 27, 28, 29, 30, 31, 32],
    [33, 34, 35, 36, 37, 38, 39, 40],
    [41, 42, 43, 44, 45, 46, 47, 48]
  ]



slice i j = take j . drop i

mid k l = slice k ((length l) - (2*k)) l

cols a = map (\i -> (map (!! i) a)) s
  where l = concat a
        n = length (head a) - 1
        s = [0..n]

col a j = cols a !! j

toList a i = v1 ++ v2 ++ reverse v3 ++ reverse v4
  where v1 = mid i $ (a!!i)
        v2 = mid (i+1) $ col a (n-i)
        v3 = mid i $ (a!!(m-i))
        v4 = mid (i+1) $ col a (i)
        m = length a - 1
        n = length (head a) - 1

toLists a = map (toList a) s
  where m = length a
        n = length (head a)
        q = min m n `div` 2
        s = [0..q-1]

moveList r l = (drop c l) ++ (take c l)
  where j = length l
        c = r `mod` j

moveLists ls r = map (moveList r) ls

fromList ls m n i = q -- concat [v1, v2, v3, v4]
  where q = length ls
        s = [0..n]
        v1 = ls !! 0
        v2 = ls !! 1
        v3 = ls !! 0
        v4 = ls !! 0

-- rotate a r = map (fromList) s
--   where ls = toLists a
--         ls' = moveLists ls r
--         m = length a
--         n = length (head a)
--         s = [0..m-1]

printM :: [[Int]] -> IO ()
printM = mapM_ (print)

main :: IO ()
main = do
    putStrLn "Original"
    printM matrix

    putStrLn "\ncols"
    print $ cols matrix

    putStrLn "\ncol"
    print $ col matrix 1

    putStrLn "\nmid"
    print $ mid 1 $ col matrix 1

    putStrLn "\ntoList 0"
    print $ toList matrix 0

    putStrLn "\ntoList 1"
    print $ toList matrix 1

    putStrLn "\ntoLists"
    printM $ toLists matrix

    putStrLn "\nmoveList"
    print $ moveList 3 (toList matrix 1)

    putStrLn "\nmoveLists"
    printM $ moveLists (toLists matrix) 3

    let m = length matrix - 1
        n = length (head matrix) - 1

    -- print $ m
    -- print $ n

    putStr "\nfromList 3: "
    print $ fromList (toLists matrix) m n 3
    putStr "fromList 4: "
    print $ fromList (toLists matrix) m n 4

    -- putStrLn "\nfromList 0"
    -- printM $ fromList (moveLists (toLists matrix) 1) 0

    -- putStrLn "\nfromList 1"
    -- printM $ fromList (moveLists (toLists matrix) 1) 1

    -- putStrLn "\nRotated"
    -- printM $ rotate matrix 1

    return ()