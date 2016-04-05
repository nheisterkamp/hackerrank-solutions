main :: IO ()
main = do
  putStrLn "Brainf*ck"

  let c = 'A'

  putStrLn $ [c]
  putStrLn $ [c]

  return ()
