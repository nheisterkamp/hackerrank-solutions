import Control.Applicative
import Control.Monad
import System.IO
import Data.Char (ord)

main :: IO ()
main = do
    h_temp <- getLine
    let h = map read $ words h_temp :: [Int]
    word <- getLine
    print $ (length word) * (maximum $ map (\x->h!!((ord x) - 97)) word)

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
