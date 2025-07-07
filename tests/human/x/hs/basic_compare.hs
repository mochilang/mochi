module Main where

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

main :: IO ()
main = do
  let a = 10 - 3
  let b = 2 + 2
  print a
  putStrLn $ showBool (a == 7)
  putStrLn $ showBool (b < 5)
