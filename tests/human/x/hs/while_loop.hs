module Main where

loop :: Int -> IO ()
loop i
  | i < 3 = do
      print i
      loop (i + 1)
  | otherwise = return ()

main :: IO ()
main = loop 0
