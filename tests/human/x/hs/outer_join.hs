module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int, total :: Int }

customers = [ Customer 1 "Alice", Customer 2 "Bob", Customer 3 "Charlie", Customer 4 "Diana" ]
orders = [ Order 100 1 250, Order 101 2 125, Order 102 1 300, Order 103 5 80 ]

lookupCustomer cid' = case [c | c <- customers, cid c == cid'] of
  (x:_) -> Just x
  [] -> Nothing

result = [ Left c | c <- customers, null [o | o <- orders, ocid o == cid c] ] ++
         [ Right (o, lookupCustomer (ocid o)) | o <- orders ]

main :: IO ()
main = do
  putStrLn "--- Outer Join using syntax ---"
  mapM_ printRow result
  where
    printRow (Right (o, Just c)) =
      printf "Order %d by %s - $%d\n" (oid o) (cname c) (total o)
    printRow (Right (o, Nothing)) =
      printf "Order %d by Unknown - $%d\n" (oid o) (total o)
    printRow (Left c) =
      printf "Customer %s has no orders\n" (cname c)
