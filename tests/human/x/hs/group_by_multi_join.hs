module Main where

import qualified Data.Map.Strict as M

-- simple records

data Nation = Nation { nid :: Int, nname :: String }

data Supplier = Supplier { sid :: Int, snation :: Int }

data PartSupp = PartSupp { part :: Int, supplier :: Int, cost :: Double, qty :: Int }

nations :: [Nation]
nations = [ Nation 1 "A", Nation 2 "B" ]

suppliers :: [Supplier]
suppliers = [ Supplier 1 1, Supplier 2 2 ]

partsupp :: [PartSupp]
partsupp = [ PartSupp 100 1 10 2, PartSupp 100 2 20 1, PartSupp 200 1 5 3 ]

filtered :: [(Int, Double)]
filtered = [ (part ps, cost ps * fromIntegral (qty ps))
           | ps <- partsupp
           , s <- suppliers, sid s == supplier ps
           , n <- nations, nid n == snation s, nname n == "A" ]

grouped :: M.Map Int Double
grouped = M.fromListWith (+) filtered

main :: IO ()
main = print [ (p, v) | (p,v) <- M.toList grouped ]
