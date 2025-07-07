module Main where

import Data.IORef

updateIndex :: Int -> a -> [a] -> [a]
updateIndex i val xs = take i xs ++ [val] ++ drop (i+1) xs

main :: IO ()
main = do
  nums <- newIORef ([1,2] :: [Int])
  modifyIORef nums (updateIndex 1 3)
  result <- readIORef nums
  print (result !! 1)
