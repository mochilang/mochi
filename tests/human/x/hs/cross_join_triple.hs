module Main where

main :: IO ()
main = do
  let nums = [1,2]
      letters = ["A","B"]
      bools = [True, False]
      combos = [ (n,l,b) | n <- nums, l <- letters, b <- bools ]
  putStrLn "--- Cross Join of three lists ---"
  mapM_ (\(n,l,b) -> putStrLn $ show n ++ " " ++ l ++ " " ++ show b) combos

