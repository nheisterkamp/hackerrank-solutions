parse [] = []
parse (nk:as:r) = as'
    where [n, k] = map read (words nk) :: [Int]
          as' = map read (words as) :: [Int]

main :: IO ()
main = do
    n <- getLine
    input <- getContents
    
    print $ parse (lines input)


