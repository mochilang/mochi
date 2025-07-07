module Main where

main :: IO ()
main = do
  let xs = [10,20,30 :: Int]
  print (xs !! 1)
