module Main where

main :: IO ()
main = putStrLn $ unwords $ map show result
  where
    a = [1,2]
    result = a ++ [3]
