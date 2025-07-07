module Main where

import qualified Data.Map.Strict as M
import Data.List (foldl')

data Item = Item { cat :: String, val :: Int, flag :: Bool }

items :: [Item]
items = [ Item "a" 10 True
        , Item "a" 5 False
        , Item "b" 20 True ]

foldItems :: M.Map String [Item]
foldItems = foldl' (\m i -> M.insertWith (++) (cat i) [i] m) M.empty items

share :: [Item] -> Double
share xs = let s = sum [val x | x <- xs, flag x]
               total = sum [val x | x <- xs]
           in fromIntegral s / fromIntegral total

main :: IO ()
main = print [M.fromList [("cat", c), ("share", share xs)] | (c,xs) <- M.toList foldItems]
