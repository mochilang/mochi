module Main where

main :: IO ()
main = do
  let dat = [1, 2]
  let flag = any (== 1) dat
  print flag
