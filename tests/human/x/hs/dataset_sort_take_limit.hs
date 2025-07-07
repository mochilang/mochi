module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

data Product = Product { pname :: String, price :: Int }

products :: [Product]
products = [ Product "Laptop" 1500
           , Product "Smartphone" 900
           , Product "Tablet" 600
           , Product "Monitor" 300
           , Product "Keyboard" 100
           , Product "Mouse" 50
           , Product "Headphones" 200 ]

main :: IO ()
main = do
  let sorted = sortBy (flip $ comparing price) products
      expensive = take 3 (drop 1 sorted)
  putStrLn "--- Top products (excluding most expensive) ---"
  mapM_ (\p -> putStrLn $ pname p ++ " costs $" ++ show (price p)) expensive

