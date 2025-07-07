module Main where

sum3 :: Int -> Int -> Int -> Int
sum3 a b c = a + b + c

main :: IO ()
main = print (sum3 1 2 3)
