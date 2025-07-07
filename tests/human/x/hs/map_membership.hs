module Main where

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let m = M.fromList [("a",1),("b",2)] :: M.Map String Int
  print (M.member "a" m)
  print (M.member "c" m)
