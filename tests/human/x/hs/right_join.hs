module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int, total :: Int }

customers = [ Customer 1 "Alice", Customer 2 "Bob", Customer 3 "Charlie", Customer 4 "Diana" ]
orders = [ Order 100 1 250, Order 101 2 125, Order 102 1 300 ]

ordersFor c = [o | o <- orders, ocid o == cid c]

result = [ (cname c, Just o) | c <- customers, o <- ordersFor c ] ++
         [ (cname c, Nothing) | c <- customers, null (ordersFor c) ]

main :: IO ()
main = do
  putStrLn "--- Right Join using syntax ---"
  mapM_ printRow result
  where
    printRow (name, Just o) =
      printf "Customer %s has order %d - $%d\n" name (oid o) (total o)
    printRow (name, Nothing) =
      printf "Customer %s has no orders\n" name
