module Main where

add :: Int -> Int -> Int
add a b = a + b

add5 :: Int -> Int
add5 = add 5

main :: IO ()
main = print (add5 3)
