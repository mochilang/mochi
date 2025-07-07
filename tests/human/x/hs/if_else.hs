module Main where

main :: IO ()
main =
  let x = 5 in
  if x > 3
    then putStrLn "big"
    else putStrLn "small"
