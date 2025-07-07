module Main where

triple :: Int -> Int
triple x = x * 3

main :: IO ()
main = print (triple (1 + 2))
