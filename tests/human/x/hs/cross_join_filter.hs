module Main where

main :: IO ()
main = do
  let nums = [1,2,3]
      letters = ["A", "B"]
      pairs = [ (n,l) | n <- nums, even n, l <- letters ]
  putStrLn "--- Even pairs ---"
  mapM_ (\(n,l) -> putStrLn $ show n ++ " " ++ l) pairs

