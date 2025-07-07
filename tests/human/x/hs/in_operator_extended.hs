module Main where

import qualified Data.Map.Strict as M
import Data.List (isInfixOf)

main :: IO ()
main = do
  let xs = [1,2,3]
      ys = filter odd xs
  print (1 `elem` ys)
  print (2 `elem` ys)
  let m = M.fromList [("a",1)] :: M.Map String Int
  print (M.member "a" m)
  print (M.member "b" m)
  let s = "hello"
  print ("ell" `isInfixOf` s)
  print ("foo" `isInfixOf` s)
