module Main where

import qualified Data.Map.Strict as M
import Data.List (sortOn)

data Item = Item { tag :: String, val :: Int }

items :: [Item]
items = [ Item "a" 1, Item "a" 2, Item "b" 3 ]

groups :: [(String, [Item])]
groups = M.toList $ M.fromListWith (++) [(tag i, [i]) | i <- items]

result :: [(String, Int)]
result = [ (t, sum [val x | x <- xs]) | (t,xs) <- groups ]

sorted :: [(String, Int)]
sorted = sortOn fst result

main :: IO ()
main = print [ M.fromList [("tag", t), ("total", s)] | (t,s) <- sorted ]
