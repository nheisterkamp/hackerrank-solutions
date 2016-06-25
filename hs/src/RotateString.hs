import Data.List (intersperse)

rotations :: String -> String
rotations s = concat $ intersperse " " (rotations' s)

rotations' :: String -> [String]
rotations' s = [drop t $ take (l+t) s' | t <- [1..l]]
  where s' = concat (repeat s)
        l = length s

main :: IO ()
main = do
  n <- getLine
  input <- getContents
  mapM_ (putStrLn . rotations) input
