{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding  (c, c_acctbal, c_address, c_comment, c_custkey, c_name, c_nationkey, c_phone, l, l_discount, l_extendedprice, l_orderkey, l_returnflag, n, n_name, n_nationkey, o, o_custkey, o_orderdate, o_orderkey, revenue)
-- Generated by Mochi transpiler v0.10.34 on 2025-07-21 21:13 GMT+7
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
data MGroup = k a MGroup {key :: k, items :: [a]}
data GenType7 = GenType7 {c_custkey :: Int, c_name :: String, c_acctbal :: Double, c_address :: String, c_phone :: String, c_comment :: String, n_name :: String} deriving (Show)
data GenType1 = GenType1 {n_nationkey :: Int, n_name :: String} deriving (Show)
data GenType2 = GenType2 {c_custkey :: Int, c_name :: String, c_acctbal :: Double, c_nationkey :: Int, c_address :: String, c_phone :: String, c_comment :: String} deriving (Show)
data GenType3 = GenType3 {o_orderkey :: Int, o_custkey :: Int, o_orderdate :: String} deriving (Show)
data GenType4 = GenType4 {l_orderkey :: Int, l_returnflag :: String, l_extendedprice :: Double, l_discount :: Double} deriving (Show)
data GenType5 = GenType5 {c_custkey :: String, c_name :: String, revenue :: String, c_acctbal :: String, n_name :: String, c_address :: String, c_phone :: String, c_comment :: String} deriving (Show)
data GenType6 = GenType6 {c :: GenType2, o :: GenType3, l :: GenType4, n :: GenType1} deriving (Show)
customer = [GenType2 {c_custkey = 1, c_name = "Alice", c_acctbal = 100, c_nationkey = 1, c_address = "123 St", c_phone = "123-456", c_comment = "Loyal"}]
end_date = "1994-01-01"
lineitem = [GenType4 {l_orderkey = 1000, l_returnflag = "R", l_extendedprice = 1000, }, GenType4 {l_orderkey = 2000, l_returnflag = "N", l_extendedprice = 500, l_discount = 0}]
nation = [GenType1 {n_nationkey = 1, n_name = "BRAZIL"}]
orders = [GenType3 {o_orderkey = 1000, o_custkey = 1, o_orderdate = "1993-10-15"}, GenType3 {o_orderkey = 2000, o_custkey = 1, o_orderdate = "1994-01-02"}]
result = sortOn (\g -> -sum [x.l.l_extendedprice * (1 - x.l.l_discount) | x <- g.items]) [GenType5 {c_custkey = g.key.c_custkey, c_name = g.key.c_name, revenue = sum [x.l.l_extendedprice * (1 - x.l.l_discount) | x <- g.items], c_acctbal = g.key.c_acctbal, n_name = g.key.n_name, c_address = g.key.c_address, c_phone = g.key.c_phone, c_comment = g.key.c_comment} | g <- [MGroup {key = k, items = [GenType6 {c = c, o = o, l = l, n = n} | c <- customer, o <- orders, l <- lineitem, n <- nation, o.o_custkey == c.c_custkey && l.l_orderkey == o.o_orderkey && n.n_nationkey == c.c_nationkey && o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R" && GenType7 {c_custkey = c.c_custkey, c_name = c.c_name, c_acctbal = c.c_acctbal, c_address = c.c_address, c_phone = c.c_phone, c_comment = c.c_comment, n_name = n.n_name} == k]} | k <- nub (map (\c o l n -> GenType7 {c_custkey = c.c_custkey, c_name = c.c_name, c_acctbal = c.c_acctbal, c_address = c.c_address, c_phone = c.c_phone, c_comment = c.c_comment, n_name = n.n_name}) [GenType6 {c = c, o = o, l = l, n = n} | c <- customer, o <- orders, l <- lineitem, n <- nation, o.o_custkey == c.c_custkey && l.l_orderkey == o.o_orderkey && n.n_nationkey == c.c_nationkey && o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R"])]]
start_date = "1993-10-01"
main :: IO ()
main = do
    print (result)
