module Main where

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let m = M.fromList [(1, "a"), (2, "b")]
  print (M.member 1 m)
  print (M.member 3 m)
