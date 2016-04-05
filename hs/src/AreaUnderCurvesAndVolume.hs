module AreaUnderCurvesAndVolume where

import Text.Printf (printf)

dx :: Double
dx = 0.001

-- This function should return a list [area, volume].
solve :: Double -> Double -> [Double] -> [Double] -> [Double]
solve l r a b = [area, volume]
  where len = (r - l) / dx
        xs = [l + x * dx | x <- [0 .. len]]
        eq x = sum $ zipWith (\a' b' -> a' * (x ** b')) a b
        area = foldr ((+) . (dx *) . eq) 0 xs
        volume = foldr ((+) . (dx * pi *) . \x -> eq x ^ 2) 0 xs


--Input/Output.
main :: IO ()
main = do
  let l = 1
      r = 4
      a = [1, 2, 3, 4, 5]
      b = [6, 7, 8, 9, 10]

  -- | Sample output
  -- 2435300.3
  -- 26172951168940.8
  mapM_ (printf "%.1f\n") $ solve l r a b
