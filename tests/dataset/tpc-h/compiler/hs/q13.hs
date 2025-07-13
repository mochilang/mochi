{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (groupBy, sortOn, isInfixOf)
import Data.Ord (Down(..))
import Data.Function (on)
import GHC.Generics (Generic)

expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"

data Customer = Customer { c_custkey :: Int } deriving (Show)
data Order = Order { o_orderkey :: Int, o_custkey :: Int, o_comment :: String } deriving (Show)

data Per = Per { c_count :: Int } deriving (Show)
data Result = Result { res_count :: Int, custdist :: Int } deriving (Show, Eq, Generic)
instance Aeson.ToJSON Result where
  toJSON (Result c d) = Aeson.object ["c_count" Aeson..= c, "custdist" Aeson..= d]

customer :: [Customer]
customer = [Customer 1, Customer 2, Customer 3]

orders :: [Order]
orders = [ Order 100 1 "fast delivery"
         , Order 101 1 "no comment"
         , Order 102 2 "special requests only"
         ]

per_customer :: [Per]
per_customer = [ Per (length [ o | o <- orders
                                , o_custkey o == c_custkey c
                                , not ("special" `isInfixOf` o_comment o)
                                , not ("requests" `isInfixOf` o_comment o)
                                ])
               | c <- customer ]


grouped :: [Result]
grouped = [ Result k (length xs)
          | let groups = groupBy ((==) `on` c_count) (sortOn (Down . c_count) per_customer)
          , xs <- groups
          , let k = c_count (head xs)
          ]

test_Q13_groups_customers_by_non_special_order_count :: IO ()
test_Q13_groups_customers_by_non_special_order_count =
  expect (grouped == [Result 2 1, Result 0 2])

main :: IO ()
main = do
  BSL.putStrLn (Aeson.encode grouped)
  test_Q13_groups_customers_by_non_special_order_count
