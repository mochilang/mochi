module Main where

import qualified Data.Map as Map

main :: IO ()
main = do
  let m = Map.fromList [("a", 1 :: Int), ("b", 2)]
  print (Map.findWithDefault 0 "b" m)
