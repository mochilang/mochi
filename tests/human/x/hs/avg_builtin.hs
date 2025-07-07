module Main where

avg :: [Int] -> Int
avg xs = sum xs `div` length xs

main :: IO ()
main = print $ avg [1,2,3]
