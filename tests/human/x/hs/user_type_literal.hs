module Main where

data Person = Person { pname :: String, age :: Int }

data Book = Book { title :: String, author :: Person }

main :: IO ()
main = do
  let book = Book "Go" (Person "Bob" 42)
  putStrLn (pname (author book))
