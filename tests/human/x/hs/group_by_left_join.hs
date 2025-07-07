module Main where

import qualified Data.Map.Strict as M

data Customer = Customer { cid :: Int, cname :: String }

data Order = Order { oid :: Int, ocid :: Int }

customers :: [Customer]
customers = [ Customer 1 "Alice", Customer 2 "Bob", Customer 3 "Charlie" ]

orders :: [Order]
orders = [ Order 100 1, Order 101 1, Order 102 2 ]

main :: IO ()
main = do
  putStrLn "--- Group Left Join ---"
  mapM_ printStat result
  where
    countOrders c = length [o | o <- orders, ocid o == cid c]
    result = [ (cname c, countOrders c) | c <- customers ]
    printStat (name,cnt) = putStrLn $ name ++ " orders: " ++ show cnt
