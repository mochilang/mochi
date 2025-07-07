module Main where

import Data.List (sortOn)

data Item = Item { a :: Int, b :: Int } deriving Show

items :: [Item]
items = [ Item 1 2, Item 1 1, Item 0 5 ]

result :: [Item]
result = sortOn (\x -> (a x, b x)) items

main :: IO ()
main = print result
