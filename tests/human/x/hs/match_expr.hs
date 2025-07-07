module Main where

main :: IO ()
main = do
  let x = 2
      label = case x of
                 1 -> "one"
                 2 -> "two"
                 3 -> "three"
                 _ -> "unknown"
  putStrLn label
