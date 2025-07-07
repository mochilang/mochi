module Main where

import qualified Data.Map as Map

main :: IO ()
main = do
  let m = Map.fromList [(1, "a"), (2, "b")] :: Map.Map Int String
  print (Map.findWithDefault "" 1 m)
