module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

data Item = Item { n :: Int, v :: String } deriving Show

items :: [Item]
items = [ Item 1 "a", Item 1 "b", Item 2 "c" ]

result = [ v i | i <- sortBy (comparing n) items ]

main :: IO ()
main = print result
