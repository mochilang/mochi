{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (age, avg_age, city, count, name)
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
data MGroup = MGroup {key :: k, items :: [a] }
data GenType1 = GenType1 {name :: String, age :: Int, city :: String }
data GenType2 = GenType2 {city :: String, count :: Int, avg_age :: Double }
people = [GenType1{name = "Alice", age = 30, city = "Paris"},
 GenType1{name = "Bob", age = 15, city = "Hanoi"},
 GenType1{name = "Charlie", age = 65, city = "Paris"},
 GenType1{name = "Diana", age = 45, city = "Hanoi"},
 GenType1{name = "Eve", age = 70, city = "Paris"},
 GenType1{name = "Frank", age = 22, city = "Hanoi"}]
stats = [GenType2{city = g . key, count = length g . items,
          avg_age =
            fromIntegral (sum [p . age | p <- g . items]) /
              fromIntegral (length [p . age | p <- g . items])}
 |
 g <- [MGroup{key = k,
              items = [person | person <- people, person . city == k]}
       | k <- nub ([person . city | person <- people])]]
main :: IO ()
main = do putStrLn "--- People grouped by city ---"
   mapM_
     (\ s ->
        do putStrLn
             (s . city ++
                " " ++
                  ": count =" ++
                    " " ++
                      show s . count ++ " " ++ ", avg_age =" ++ " " ++ show s . avg_age))
     stats
