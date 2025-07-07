module Main where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..), comparing)

data Nation = Nation { nk :: Int, nname :: String }

data Customer = Customer { ck :: Int, cname :: String, cacct :: Double, caddr :: String, cphone :: String, ccomment :: String, cnation :: Int }

data Order = Order { ok :: Int, ocust :: Int, odate :: String }

data Line = Line { lorder :: Int, lflag :: String, lext :: Double, ldisc :: Double }

nation :: [Nation]
nation = [ Nation 1 "BRAZIL" ]

customer :: [Customer]
customer = [ Customer 1 "Alice" 100 "123 St" "123-456" "Loyal" 1 ]

orders :: [Order]
orders = [ Order 1000 1 "1993-10-15", Order 2000 1 "1994-01-02" ]

lineitem :: [Line]
lineitem = [ Line 1000 "R" 1000 0.1, Line 2000 "N" 500 0.0 ]

startDate, endDate :: String
startDate = "1993-10-01"
endDate = "1994-01-01"

rows = [ (c,o,l,n)
       | c <- customer
       , o <- orders, ocust o == ck c
       , l <- lineitem, lorder l == ok o
       , n <- nation, nk n == cnation c
       , odate o >= startDate
       , odate o < endDate
       , lflag l == "R" ]

revenue m = sum [ lext l * (1 - ldisc l) | (_,_,l,_) <- m ]

key (c,_,_,n) = ( ck c, cname c, cacct c, caddr c, cphone c, ccomment c, nname n )

grouped = M.fromListWith (++) [ (key row, [row]) | row <- rows ]

result = [ (k, revenue rs) | (k, rs) <- M.toList grouped ]

sorted = sortBy (comparing (Down . snd)) result

main :: IO ()
main = print [ (custKey, name, rev) | ((custKey,name,_,_,_,_,_), rev) <- sorted ]
