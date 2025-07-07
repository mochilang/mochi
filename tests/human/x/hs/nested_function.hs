module Main where

outer :: Int -> Int
outer x =
  let inner y = x + y
  in inner 5

main :: IO ()
main = print (outer 3)
