module Main where

main :: IO ()
main = do
  let xs = [1,2,3]
  print (2 `elem` xs)
  print (not (5 `elem` xs))
