input' =
  [ "3"
  , "1043 770"
  , "551 990"
  , "681 463" 
  ]

solve :: [(Double,Double)] -> Double
solve ps = solve' $ zip (ps) (tail ps ++ [head ps])

solve' :: [((Double,Double),(Double,Double))] -> Double
solve' ps = (foldl (\r ((ax,ay),(bx,by)) -> 
                r + ((ax*by)-(ay*bx))) 0 ps) / 2

main :: IO ()
main = do
  let input = unlines input'

  let ps = map ((\(x:y:_) -> 
                (read x,read y) :: (Double,Double)) . words) $
               tail (lines input)

  print $ solve ps
