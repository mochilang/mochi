{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub, sortOn)
import Data.Ord (Down(..))
import GHC.Generics (Generic)

-- simple assertion
expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"

data Nation = Nation { n_nationkey :: Int, n_name :: String } deriving (Show)
data Supplier = Supplier { s_suppkey :: Int, s_nationkey :: Int } deriving (Show)
data Partsupp = Partsupp { ps_partkey :: Int, ps_suppkey :: Int, ps_supplycost :: Double, ps_availqty :: Int } deriving (Show)

data Result = Result { res_partkey :: Int, value :: Double } deriving (Show, Eq, Generic)
instance Aeson.ToJSON Result where
  toJSON (Result pk v) = Aeson.object ["ps_partkey" Aeson..= pk, "value" Aeson..= v]

nation :: [Nation]
nation = [Nation 1 "GERMANY", Nation 2 "FRANCE"]

supplier :: [Supplier]
supplier = [Supplier 100 1, Supplier 200 1, Supplier 300 2]

partsupp :: [Partsupp]
partsupp = [Partsupp 1000 100 10.0 100, Partsupp 1000 200 20.0 50, Partsupp 2000 100 5.0 10, Partsupp 3000 300 8.0 500]

target_nation :: String
target_nation = "GERMANY"

filtered :: [Result]
filtered = [ Result (ps_partkey ps) (ps_supplycost ps * fromIntegral (ps_availqty ps))
           | ps <- partsupp
           , s <- supplier, s_suppkey s == ps_suppkey ps
           , n <- nation, n_nationkey n == s_nationkey s, n_name n == target_nation
           ]

partKeys :: [Int]
partKeys = nub [pk | Result pk _ <- filtered]

grouped :: [(Int, Double)]
grouped = [ (pk, sum [v | Result pk' v <- filtered, pk' == pk]) | pk <- partKeys ]

total :: Double
total = sum [v | Result _ v <- filtered]

threshold :: Double
threshold = total * 0.0001

result :: [Result]
result = sortOn (Down . value) [Result pk v | (pk,v) <- grouped, v > threshold]

test_Q11_returns_high_value_partkeys_from_GERMANY :: IO ()
test_Q11_returns_high_value_partkeys_from_GERMANY =
  expect (result == [Result 1000 2000.0, Result 2000 50.0])

main :: IO ()
main = do
  BSL.putStrLn (Aeson.encode result)
  test_Q11_returns_high_value_partkeys_from_GERMANY
