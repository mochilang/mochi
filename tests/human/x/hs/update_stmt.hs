data Person = Person { name :: String, age :: Int, status :: String } deriving (Eq, Show)

updatePerson :: Person -> Person
updatePerson p
  | age p >= 18 = p { status = "adult", age = age p + 1 }
  | otherwise = p

people0 :: [Person]
people0 = [ Person "Alice" 17 "minor"
          , Person "Bob" 25 "unknown"
          , Person "Charlie" 18 "unknown"
          , Person "Diana" 16 "minor"
          ]

expected :: [Person]
expected = [ Person "Alice" 17 "minor"
           , Person "Bob" 26 "adult"
           , Person "Charlie" 19 "adult"
           , Person "Diana" 16 "minor"
           ]

main :: IO ()
main = do
  let people1 = map updatePerson people0
  if people1 == expected
    then putStrLn "ok"
    else putStrLn "fail"
