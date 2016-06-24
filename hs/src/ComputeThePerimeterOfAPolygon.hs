input' =
  [ "4"
  , "0 0"
  , "0 1"  
  , "1 1"  
  , "1 0"]


solve :: [(Double,Double)] -> Double
solve ps = solve' $ zip (ps) (tail ps ++ [head ps])

solve' :: [((Double,Double),(Double,Double))] -> Double
solve' = foldl (\r ((ax,ay),(bx,by)) -> 
                r + (sqrt ((ax-bx)**2+(ay-by)**2))) 0

main :: IO ()
main = do
  let input = unlines input'

  let ps = map ((\(x:y:_) -> 
                (read x,read y) :: (Double,Double)) . words) $
               tail (lines input)

  print $ solve ps
