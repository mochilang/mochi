{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (sortOn)
import GHC.Generics (Generic)

expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"

data Order = Order { o_orderkey :: Int, o_orderpriority :: String } deriving (Show)
data Lineitem = Lineitem { l_orderkey :: Int, l_shipmode :: String, l_commitdate :: String, l_receiptdate :: String, l_shipdate :: String } deriving (Show)

data Result = Result { shipmode :: String, high_line_count :: Int, low_line_count :: Int } deriving (Show, Eq, Generic)
instance Aeson.ToJSON Result where
  toJSON (Result m h l) = Aeson.object ["l_shipmode" Aeson..= m, "high_line_count" Aeson..= h, "low_line_count" Aeson..= l]

orders :: [Order]
orders = [Order 1 "1-URGENT", Order 2 "3-MEDIUM"]

lineitem :: [Lineitem]
lineitem = [ Lineitem 1 "MAIL" "1994-02-10" "1994-02-15" "1994-02-05"
           , Lineitem 2 "SHIP" "1994-03-01" "1994-02-28" "1994-02-27"
           ]

within :: String -> String -> String -> Bool
within d start end = d >= start && d < end

filtered :: [(Lineitem, Order)]
filtered = [ (l,o) | l <- lineitem, o <- orders, o_orderkey o == l_orderkey l
                   , l_shipmode l `elem` ["MAIL","SHIP"]
                   , l_commitdate l < l_receiptdate l
                   , l_shipdate l < l_commitdate l
                   , within (l_receiptdate l) "1994-01-01" "1995-01-01" ]

grouped :: [(String, [(Lineitem,Order)])]
grouped = [(m, [p | p@(l,_) <- filtered, l_shipmode l == m]) | m <- ["MAIL","SHIP"], any ((\l -> l_shipmode l == m) . fst) filtered]

result :: [Result]
result = sortOn shipmode [ Result m high low | (m,rows) <- grouped
                      , let high = length [() | (_ ,o) <- rows, o_orderpriority o `elem` ["1-URGENT","2-HIGH"]]
                      , let low  = length [() | (_ ,o) <- rows, not (o_orderpriority o `elem` ["1-URGENT","2-HIGH"])]
                      ]

test_Q12_counts_lineitems_by_ship_mode_and_priority :: IO ()
test_Q12_counts_lineitems_by_ship_mode_and_priority =
  expect (result == [Result "MAIL" 1 0])

main :: IO ()
main = do
  BSL.putStrLn (Aeson.encode result)
  test_Q12_counts_lineitems_by_ship_mode_and_priority
