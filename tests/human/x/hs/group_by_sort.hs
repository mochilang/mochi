module Main where

import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Ord (Down(..))

data Item = Item { cat :: String, val :: Int }

items :: [Item]
items = [ Item "a" 3, Item "a" 1, Item "b" 5, Item "b" 2 ]

aggregated :: [(String, Int)]
aggregated = M.toList $ M.fromListWith (+) [(cat i, val i) | i <- items]

sorted :: [(String, Int)]
sorted = sortOn (Down . snd) aggregated

main :: IO ()
main = print [ M.fromList [("cat", c),("total", t)] | (c,t) <- sorted ]
