module Main where

loop :: [Int] -> IO ()
loop [] = return ()
loop (n:ns)
  | n > 7 = return ()
  | even n = loop ns
  | otherwise = do
      putStrLn $ "odd number: " ++ show n
      loop ns

main :: IO ()
main = loop [1..9]
