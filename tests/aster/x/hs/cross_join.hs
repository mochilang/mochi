{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (customerId, id, name, orderCustomerId, orderId, orderTotal, pairedCustomerName, total)
data GenType1 = GenType1 {id :: Int, name :: String }
data GenType2 = GenType2 {id :: Int, customerId :: Int, total :: Int }
data GenType3 = GenType3 {orderId :: Int, orderCustomerId :: Int, pairedCustomerName :: String, orderTotal :: Int }
customers = [GenType1{id = 1, name = "Alice"}, GenType1{id = 2, name = "Bob"},
 GenType1{id = 3, name = "Charlie"}]
orders = [GenType2{id = 100, customerId = 1, total = 250},
 GenType2{id = 101, customerId = 2, total = 125},
 GenType2{id = 102, customerId = 1, total = 300}]
result = [GenType3{orderId = o . id, orderCustomerId = o . customerId,
          pairedCustomerName = c . name, orderTotal = o . total}
 | o <- orders, c <- customers]
main :: IO ()
main = do putStrLn "--- Cross Join: All order-customer pairs ---"
   mapM_
     (\ entry ->
        do putStrLn
             ("Order" ++
                " " ++
                  show entry . orderId ++
                    " " ++
                      "(customerId:" ++
                        " " ++
                          show entry . orderCustomerId ++
                            " " ++
                              ", total: $" ++
                                " " ++
                                  show entry . orderTotal ++
                                    " " ++ ") paired with" ++ " " ++ entry . pairedCustomerName))
     result
