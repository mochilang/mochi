module Main where

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

main :: IO ()
main = do
  putStrLn $ showBool ("a" < "b")
  putStrLn $ showBool ("a" <= "a")
  putStrLn $ showBool ("b" > "a")
  putStrLn $ showBool ("b" >= "b")
