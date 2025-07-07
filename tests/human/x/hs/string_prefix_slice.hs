module Main where

main :: IO ()
main = do
  let prefix = "fore"
  let s1 = "forest"
  print (take (length prefix) s1 == prefix)
  let s2 = "desert"
  print (take (length prefix) s2 == prefix)
