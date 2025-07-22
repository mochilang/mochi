{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (customer, customerId, id, name, orderId, total)

data GenType1 = GenType1 { id :: Int, name :: String } deriving (Show, Eq)

data GenType2 = GenType2 { id :: Int, customerId :: Int, total :: Int } deriving (Show, Eq)

data GenType3 = GenType3 { orderId :: Int, customer :: Maybe GenType1, total :: Int } deriving (Show, Eq)

customers = [GenType1 {id = 1, name = "Alice"}, GenType1 {id = 2, name = "Bob"}]
orders = [GenType2 {id = 100, customerId = 1, total = 250}, GenType2 {id = 101, customerId = 3, total = 80}]
result = [ GenType3 { orderId = o.id, customer = c, total = o.total }
         | o <- orders
         , c <- let ms0 = [ c | c <- customers, o.customerId == c.id ]
                  in if null ms0 then [Nothing] else map Just ms0]

main :: IO ()
main = do
    putStrLn "--- Left Join ---"
    mapM_ (\entry ->
        putStrLn ("Order " ++ show entry.orderId ++ " customer " ++ show entry.customer ++ " total " ++ show entry.total)
        ) result
