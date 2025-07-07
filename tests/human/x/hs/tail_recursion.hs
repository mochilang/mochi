module Main where

sumRec :: Int -> Int -> Int
sumRec 0 acc = acc
sumRec n acc = sumRec (n-1) (acc+n)

main :: IO ()
main = print (sumRec 10 0)
