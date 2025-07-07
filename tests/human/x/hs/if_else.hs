main :: IO ()
main = do
  let x = 5
  if x > (3 :: Int)
    then putStrLn "big"
    else putStrLn "small"
