module Main where

main :: IO ()
main = mapM_ print results
  where
    results = [1 + 2 * 3, (1 + 2) * 3, 2 * 3 + 1, 2 * (3 + 1)]
