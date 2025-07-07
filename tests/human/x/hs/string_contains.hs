module Main where

import Data.List (isInfixOf)

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

main :: IO ()
main = do
  let s = "catch"
  putStrLn $ showBool ("cat" `isInfixOf` s)
  putStrLn $ showBool ("dog" `isInfixOf` s)
