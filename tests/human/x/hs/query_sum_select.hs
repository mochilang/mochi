module Main where

nums :: [Int]
nums = [1,2,3]

result :: Int
result = sum [n | n <- nums, n > 1]

main :: IO ()
main = print result
