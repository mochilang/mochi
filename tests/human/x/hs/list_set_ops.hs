module Main where

import Data.List (nub, (\\), intersect)

main :: IO ()
main = do
  print (nub ([1,2] ++ [2,3]))
  print ([1,2,3] \\ [2])
  print ([1,2,3] `intersect` [2,4])
  print (length ([1,2] ++ [2,3]))
