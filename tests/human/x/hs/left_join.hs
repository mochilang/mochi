module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int, total :: Int }

customers :: [Customer]
customers = [ Customer 1 "Alice", Customer 2 "Bob" ]

orders :: [Order]
orders = [ Order 100 1 250, Order 101 3 80 ]

lookupCustomer :: Int -> Maybe Customer
lookupCustomer cid' = case [c | c <- customers, cid c == cid'] of
  (x:_) -> Just x
  []    -> Nothing

result = [ (oid o, lookupCustomer (ocid o), total o) | o <- orders ]

main :: IO ()
main = do
  putStrLn "--- Left Join ---"
  mapM_ printRow result
  where
    printRow (oid', mcust, tot) =
      printf "Order %d customer %s total %d\n" oid' (show mcust) tot
