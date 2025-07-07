module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int }

data Item = Item { orderId :: Int, sku :: String }

customers = [ Customer 1 "Alice", Customer 2 "Bob" ]
orders = [ Order 100 1, Order 101 2 ]
items = [ Item 100 "a" ]

lookupItem oid' = case [i | i <- items, orderId i == oid'] of
  (x:_) -> Just x
  []    -> Nothing

result = [ (oid o, cname c, lookupItem (oid o))
         | o <- orders, c <- customers, cid c == ocid o ]

main :: IO ()
main = do
  putStrLn "--- Left Join Multi ---"
  mapM_ (\(oId,name,it) -> printf "%d %s %s\n" oId name (show it)) result
