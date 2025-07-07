module Main where

import qualified Data.Map.Strict as M
import Data.List (foldl')
import Text.Printf (printf)

data Person = Person { name :: String, age :: Int, city :: String }

people :: [Person]
people = [ Person "Alice" 30 "Paris"
         , Person "Bob" 15 "Hanoi"
         , Person "Charlie" 65 "Paris"
         , Person "Diana" 45 "Hanoi"
         , Person "Eve" 70 "Paris"
         , Person "Frank" 22 "Hanoi" ]

avg :: [Int] -> Double
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

groupByCity :: [Person] -> M.Map String [Int]
groupByCity = foldl' (\m p -> M.insertWith (++) (city p) [age p] m) M.empty

main :: IO ()
main = do
  let stats = groupByCity people
  putStrLn "--- People grouped by city ---"
  mapM_ (\(c,ages) -> printf "%s : count = %d , avg_age = %f\n" c (length ages) (avg ages))
        (M.toList stats)
