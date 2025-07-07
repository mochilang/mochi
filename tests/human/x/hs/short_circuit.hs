module Main where

import Debug.Trace (trace)

boom :: Int -> Int -> Bool
boom _ _ = trace "boom" True

main :: IO ()
main = do
  print (False && boom 1 2)
  print (True || boom 1 2)
