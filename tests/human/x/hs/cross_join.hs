module Main where

import Text.Printf (printf)

data Customer = Customer { cid :: Int, cname :: String }
data Order = Order { oid :: Int, ocid :: Int, total :: Int }

type Entry = (Int, Int, String, Int)

customers :: [Customer]
customers = [ Customer 1 "Alice"
            , Customer 2 "Bob"
            , Customer 3 "Charlie" ]

orders :: [Order]
orders = [ Order 100 1 250
         , Order 101 2 125
         , Order 102 1 300 ]

result :: [Entry]
result = [ (oid o, ocid o, cname c, total o)
         | o <- orders, c <- customers ]

main :: IO ()
main = do
  putStrLn "--- Cross Join: All order-customer pairs ---"
  mapM_ (\(oId,cId,name,tot) ->
          printf "Order %d (customerId: %d, total: $%d) paired with %s\n" oId cId tot name)
        result

