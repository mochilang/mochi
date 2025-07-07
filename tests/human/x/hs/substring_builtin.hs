module Main where

substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

main :: IO ()
main = putStrLn (substring 1 4 "mochi")
