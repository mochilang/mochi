module Main where

main :: IO ()
main = do
  let x = 1 + 2
  if x == 3 then putStrLn "ok" else error "test failed"
