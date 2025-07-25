{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (customerId, id, name, total)
-- Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:46 GMT+7
import qualified Data.Map as Map
data GenType1 = GenType1
  { id :: Int,
    name :: String
  } deriving (Show)


data GenType2 = GenType2
  { id :: Int,
    customerId :: Int,
    total :: Int
  } deriving (Show)


customers = [GenType1 {id = 1, name = "Alice"}, GenType1 {id = 2, name = "Bob"}, GenType1 {id = 3, name = "Charlie"}, GenType1 {id = 4, name = "Diana"}]

orders = [GenType2 {id = 100, customerId = 1, total = 250}, GenType2 {id = 101, customerId = 2, total = 125}, GenType2 {id = 102, customerId = 1, total = 300}]

result = [Map.fromList [(customerName, c.name), (order, o)] | c <- customers, o <- orders, o.customerId == c.id]

main :: IO ()
main = do
    putStrLn "--- Right Join using syntax ---"
    mapM_ (\entry -> do
        if entry.order then do
        putStrLn "Customer" ++ " " ++ show entry.customerName ++ " " ++ "has order" ++ " " ++ show entry.order.id ++ " " ++ "- $" ++ " " ++ show entry.order.total
    else do
        putStrLn "Customer" ++ " " ++ show entry.customerName ++ " " ++ "has no orders"

        ) result
