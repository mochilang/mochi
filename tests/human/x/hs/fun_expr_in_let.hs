module Main where

main :: IO ()
main = do
  let square = \x -> x * x
  print (square 6)
