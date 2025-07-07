module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int }

data Item = Item { orderId :: Int, sku :: String }

customers :: [Customer]
customers = [ Customer 1 "Alice", Customer 2 "Bob" ]

orders :: [Order]
orders = [ Order 100 1, Order 101 2 ]

items :: [Item]
items = [ Item 100 "a", Item 101 "b" ]

result = [ (cname c, sku i)
         | o <- orders
         , c <- customers, cid c == ocid o
         , i <- items, orderId i == oid o ]

main :: IO ()
main = do
  putStrLn "--- Multi Join ---"
  mapM_ (\(name, s) -> printf "%s bought item %s\n" name s) result
