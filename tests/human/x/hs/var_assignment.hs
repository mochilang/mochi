module Main where

import Data.IORef

main :: IO ()
main = do
  x <- newIORef (1 :: Int)
  writeIORef x 2
  val <- readIORef x
  print val
