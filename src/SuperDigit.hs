module Main where

import Debug.Trace
import Data.Char

-- n :: Integer
-- n = 148

-- k :: Integer
-- k = 3

-- superDigit :: Integer -> Integer
-- superDigit s | trace ("superDigit " ++ show s) False = undefined
superDigit s
    | s < 10 = s
    | otherwise = superDigit $ sum $ map digitToInt (show s)

solve n k = superDigit $ (read $ concat $ replicate k (show n)) :: Int

main = do
    input <- getLine
    let parts = words input
        n = read (parts !! 0) :: Int
        k = read (parts !! 1) :: Int

    print $ solve n k
