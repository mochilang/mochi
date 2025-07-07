module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int, total :: Int }

customers :: [Customer]
customers = [ Customer 1 "Alice", Customer 2 "Bob", Customer 3 "Charlie" ]

orders :: [Order]
orders = [ Order 100 1 250, Order 101 2 125, Order 102 1 300, Order 103 4 80 ]

result = [ (oid o, cname c, total o)
         | o <- orders
         , c <- customers
         , cid c == ocid o ]

main :: IO ()
main = do
  putStrLn "--- Orders with customer info ---"
  mapM_ (\(oId,name,tot) -> printf "Order %d by %s - $%d\n" oId name tot) result
