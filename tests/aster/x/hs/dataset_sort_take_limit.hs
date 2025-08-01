{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (name, price)
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
data GenType1 = GenType1 {name :: String, price :: Int }
expensive = sortOn (\ p -> -p . price) [p | p <- products]
products = [GenType1{name = "Laptop", price = 1500},
 GenType1{name = "Smartphone", price = 900},
 GenType1{name = "Tablet", price = 600},
 GenType1{name = "Monitor", price = 300},
 GenType1{name = "Keyboard", price = 100},
 GenType1{name = "Mouse", price = 50},
 GenType1{name = "Headphones", price = 200}]
main :: IO ()
main = do putStrLn "--- Top products (excluding most expensive) ---"
   mapM_
     (\ item ->
        do putStrLn show item . name ++
             " " ++ "costs $" ++ " " ++ show item . price)
     expensive
