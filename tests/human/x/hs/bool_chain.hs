module Main where

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

boom :: IO Bool
boom = do
  putStrLn "boom"
  return True

main :: IO ()
main = do
  putStrLn $ showBool $ (1 < 2) && (2 < 3) && (3 < 4)
  -- boom should not run here due to short-circuit
  if (1 < 2) && (2 > 3)
    then boom >>= (putStrLn . showBool)
    else putStrLn "false"
  if (1 < 2) && (2 < 3) && (3 > 4)
    then boom >>= (putStrLn . showBool)
    else putStrLn "false"
