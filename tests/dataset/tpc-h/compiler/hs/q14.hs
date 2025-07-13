{-# LANGUAGE DeriveGeneric #-}
module Main where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isInfixOf)
import GHC.Generics (Generic)

expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"

data Part = Part { p_partkey :: Int, p_type :: String } deriving (Show)
data Lineitem = Lineitem { l_partkey :: Int, l_extendedprice :: Double, l_discount :: Double, l_shipdate :: String } deriving (Show)

part :: [Part]
part = [Part 1 "PROMO LUXURY", Part 2 "STANDARD BRASS"]

lineitem :: [Lineitem]
lineitem = [ Lineitem 1 1000.0 0.1 "1995-09-05"
           , Lineitem 2 800.0 0.0 "1995-09-20"
           , Lineitem 1 500.0 0.2 "1995-10-02"
           ]

start_date, end_date :: String
start_date = "1995-09-01"
end_date   = "1995-10-01"

filtered :: [(Bool, Double)]
filtered = [ ("PROMO" `isInfixOf` p_type p, l_extendedprice l * (1 - l_discount l))
           | l <- lineitem, p <- part, p_partkey p == l_partkey l
           , l_shipdate l >= start_date, l_shipdate l < end_date]

promo_sum :: Double
promo_sum = sum [rev | (promo, rev) <- filtered, promo]

total_sum :: Double
total_sum = sum [rev | (_, rev) <- filtered]

result :: Double
result = 100.0 * promo_sum / total_sum

test_Q14_calculates_promo_revenue_percent_in_1995_09 :: IO ()
test_Q14_calculates_promo_revenue_percent_in_1995_09 =
  let expected = 100.0 * (1000.0 * 0.9) / (900 + 800.0)
   in expect (abs (result - expected) < 1e-9)

main :: IO ()
main = do
  BSL.putStrLn (Aeson.encode result)
  test_Q14_calculates_promo_revenue_percent_in_1995_09
