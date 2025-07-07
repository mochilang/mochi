module Main where

main :: IO ()
main = do
  let nums = [1,2,3]
  print (2 `elem` nums)
  print (4 `elem` nums)
