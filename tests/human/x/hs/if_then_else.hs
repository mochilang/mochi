module Main where

main :: IO ()
main = do
  let x = 12
  let msg = if x > 10 then "yes" else "no"
  putStrLn msg
