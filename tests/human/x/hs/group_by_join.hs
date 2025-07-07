module Main where

import qualified Data.Map.Strict as M

data Customer = Customer { cid :: Int, cname :: String }
  deriving (Show)

data Order = Order { oid :: Int, ocid :: Int }
  deriving (Show)

customers :: [Customer]
customers = [ Customer 1 "Alice", Customer 2 "Bob" ]

orders :: [Order]
orders = [ Order 100 1, Order 101 1, Order 102 2 ]

main :: IO ()
main = do
  putStrLn "--- Orders per customer ---"
  mapM_ printStat (M.toList stats)
  where
    stats = foldr countOrder M.empty orders
    countOrder o m =
      let name = cname $ head [c | c <- customers, cid c == ocid o]
      in M.insertWith (+) name 1 m
    printStat (name,cnt) = putStrLn $ name ++ " orders: " ++ show cnt
