module Main where

main :: IO ()
main = do
  print (take 2 (drop 1 [1,2,3]))
  print (take 2 [1,2,3])
  print (take 3 (drop 1 "hello"))
