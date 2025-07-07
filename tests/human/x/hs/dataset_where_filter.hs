module Main where

data Person = Person { pname :: String, age :: Int }

people :: [Person]
people = [ Person "Alice" 30
         , Person "Bob" 15
         , Person "Charlie" 65
         , Person "Diana" 45 ]

main :: IO ()
main = do
  let adults = [ (pname p, age p, age p >= 60) | p <- people, age p >= 18 ]
  putStrLn "--- Adults ---"
  mapM_ (\(n,a,isS) ->
          putStrLn $ n ++ " is " ++ show a ++ (if isS then " (senior)" else "")) adults

