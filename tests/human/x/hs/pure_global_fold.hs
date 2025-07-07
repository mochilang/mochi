module Main where

k :: Int
k = 2

inc :: Int -> Int
inc x = x + k

main :: IO ()
main = print (inc 3)
