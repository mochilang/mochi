-- Generated by Mochi compiler v0.10.25 on 2025-07-13T05:05:11Z
-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i
      | i < end =
          case f i of
            Just v -> Just v
            Nothing -> go (i + 1)
      | otherwise = Nothing

whileLoop :: (() -> Bool) -> (() -> Maybe a) -> Maybe a
whileLoop cond body = go ()
  where
    go _
      | cond () =
          case body () of
            Just v -> Just v
            Nothing -> go ()
      | otherwise = Nothing

avg :: (Integral a) => [a] -> a
avg xs
  | null xs = 0
  | otherwise = div (sum xs) (fromIntegral (length xs))

data MGroup k a = MGroup {key :: k, items :: [a]} deriving (Show)

_group_by :: (Ord k) => [a] -> (a -> k) -> [MGroup k a]
_group_by src keyfn =
  let go [] m order = (m, order)
      go (x : xs) m order =
        let k = keyfn x
         in case Map.lookup k m of
              Just is -> go xs (Map.insert k (is ++ [x]) m) order
              Nothing -> go xs (Map.insert k [x] m) (order ++ [k])
      (m, order) = go src Map.empty []
   in [MGroup k (fromMaybe [] (Map.lookup k m)) | k <- order]

_indexString :: String -> Int -> String
_indexString s i =
  let idx = if i < 0 then i + length s else i
   in if idx < 0 || idx >= length s
        then error "index out of range"
        else [s !! idx]

_append :: [a] -> a -> [a]
_append xs x = xs ++ [x]

_input :: IO String
_input = getLine

_now :: IO Int
_now = fmap round getPOSIXTime

_readInput :: Maybe String -> IO String
_readInput Nothing = getContents
_readInput (Just p)
  | null p || p == "-" = getContents
  | otherwise = readFile p

_writeOutput :: Maybe String -> String -> IO ()
_writeOutput mp text = case mp of
  Nothing -> putStr text
  Just p
    | null p || p == "-" -> putStr text
    | otherwise -> writeFile p text

_split :: Char -> String -> [String]
_split _ "" = [""]
_split d s =
  let (h, t) = break (== d) s
   in h : case t of
        [] -> []
        (_ : rest) -> _split d rest

_parseCSV :: String -> Bool -> Char -> [Map.Map String String]
_parseCSV text header delim =
  let ls = filter (not . null) (lines text)
   in if null ls
        then []
        else
          let heads =
                if header
                  then _split delim (head ls)
                  else ["c" ++ show i | i <- [0 .. length (_split delim (head ls)) - 1]]
              start = if header then 1 else 0
              row line =
                let parts = _split delim line
                 in Map.fromList
                      [ (heads !! j, if j < length parts then parts !! j else "")
                        | j <- [0 .. length heads - 1]
                      ]
           in map row (drop start ls)

_json :: (Aeson.ToJSON a) => a -> IO ()
_json v = BSL.putStrLn (Aeson.encode v)

expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"

lineitem = [Map.fromList [("l_quantity", (17 :: Int)), ("l_extendedprice", (1000.0 :: Double)), ("l_discount", (0.05 :: Double)), ("l_tax", (0.07 :: Double)), ("l_returnflag", "N"), ("l_linestatus", "O"), ("l_shipdate", "1998-08-01")], Map.fromList [("l_quantity", (36 :: Int)), ("l_extendedprice", (2000.0 :: Double)), ("l_discount", (0.1 :: Double)), ("l_tax", (0.05 :: Double)), ("l_returnflag", "N"), ("l_linestatus", "O"), ("l_shipdate", "1998-09-01")], Map.fromList [("l_quantity", (25 :: Int)), ("l_extendedprice", (1500.0 :: Double)), ("l_discount", (0.0 :: Double)), ("l_tax", (0.08 :: Double)), ("l_returnflag", "R"), ("l_linestatus", "F"), ("l_shipdate", "1998-09-03")]]

result = [Map.fromList [("returnflag", fromMaybe (error "missing") (Map.lookup "returnflag" (key (g)))), ("linestatus", fromMaybe (error "missing") (Map.lookup "linestatus" (key (g)))), ("sum_qty", (sum [fromMaybe (error "missing") (Map.lookup "l_quantity" (x)) | x <- g] :: Double)), ("sum_base_price", (sum [fromMaybe (error "missing") (Map.lookup "l_extendedprice" (x)) | x <- g] :: Double)), ("sum_disc_price", (sum [(fromMaybe (error "missing") (Map.lookup "l_extendedprice" (x)) * ((1 - _asInt (fromMaybe (error "missing") (Map.lookup "l_discount" (x)))))) | x <- g] :: Double)), ("sum_charge", (sum [((fromMaybe (error "missing") (Map.lookup "l_extendedprice" (x)) * ((1 - _asInt (fromMaybe (error "missing") (Map.lookup "l_discount" (x)))))) * ((1 + _asInt (fromMaybe (error "missing") (Map.lookup "l_tax" (x)))))) | x <- g] :: Double)), ("avg_qty", ((sum [fromMaybe (error "missing") (Map.lookup "l_quantity" (x)) | x <- g] `div` length [fromMaybe (error "missing") (Map.lookup "l_quantity" (x)) | x <- g]) :: Double)), ("avg_price", ((sum [fromMaybe (error "missing") (Map.lookup "l_extendedprice" (x)) | x <- g] `div` length [fromMaybe (error "missing") (Map.lookup "l_extendedprice" (x)) | x <- g]) :: Double)), ("avg_disc", ((sum [fromMaybe (error "missing") (Map.lookup "l_discount" (x)) | x <- g] `div` length [fromMaybe (error "missing") (Map.lookup "l_discount" (x)) | x <- g]) :: Double)), ("count_order", (length (items g) :: Int))] | g <- _group_by [(row) | row <- lineitem, (fromMaybe (error "missing") (Map.lookup "l_shipdate" row) <= "1998-09-02")] (\(row) -> Map.fromList [("returnflag", fromMaybe (error "missing") (Map.lookup "l_returnflag" row)), ("linestatus", fromMaybe (error "missing") (Map.lookup "l_linestatus" row))]), let g = g]

test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus :: IO ()
test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus = do
  expect ((result == [Map.fromList [("returnflag", "N"), ("linestatus", "O"), ("sum_qty", (53 :: Int)), ("sum_base_price", (3000 :: Int)), ("sum_disc_price", ((950.0 + 1800.0) :: Double)), ("sum_charge", ((((950.0 * 1.07)) + ((1800.0 * 1.05))) :: Double)), ("avg_qty", (26.5 :: Double)), ("avg_price", (1500 :: Int)), ("avg_disc", (0.07500000000000001 :: Double)), ("count_order", (2 :: Int))]]))

main :: IO ()
main = do
  _json result
  test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus
