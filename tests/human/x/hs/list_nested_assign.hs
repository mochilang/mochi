module Main where

import Data.IORef

updateMatrix :: Int -> Int -> a -> [[a]] -> [[a]]
updateMatrix i j val mat =
  let row = mat !! i
      row' = take j row ++ [val] ++ drop (j+1) row
  in take i mat ++ [row'] ++ drop (i+1) mat

main :: IO ()
main = do
  matrixRef <- newIORef ([[1,2],[3,4 :: Int]] :: [[Int]])
  modifyIORef matrixRef (updateMatrix 1 0 5)
  matrix <- readIORef matrixRef
  print ((matrix !! 1) !! 0)
