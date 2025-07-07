module Main where

data Person = Person { name :: String, age :: Int, status :: String } deriving (Eq, Show)

people :: [Person]
people = [ Person "Alice" 17 "minor"
         , Person "Bob" 25 "unknown"
         , Person "Charlie" 18 "unknown"
         , Person "Diana" 16 "minor" ]

update :: [Person] -> [Person]
update = map f
  where
    f p | age p >= 18 = p { status = "adult", age = age p + 1 }
        | otherwise   = p

expected :: [Person]
expected = [ Person "Alice" 17 "minor"
           , Person "Bob" 26 "adult"
           , Person "Charlie" 19 "adult"
           , Person "Diana" 16 "minor" ]

main :: IO ()
main = do
  let updated = update people
  if updated == expected then putStrLn "ok" else print updated
