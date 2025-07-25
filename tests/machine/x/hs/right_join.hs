-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf, isInfixOf)
import qualified Data.List as List


forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i | i < end =
            case f i of
              Just v -> Just v
              Nothing -> go (i + 1)
         | otherwise = Nothing

whileLoop :: (() -> Bool) -> (() -> Maybe a) -> Maybe a
whileLoop cond body = go ()
  where
    go _ | cond () =
            case body () of
              Just v -> Just v
              Nothing -> go ()
         | otherwise = Nothing

avg :: Integral a => [a] -> a
avg xs | null xs = 0
       | otherwise = div (sum xs) (fromIntegral (length xs))

data MGroup k a = MGroup { key :: k, items :: [a] } deriving (Show)

_group_by :: Ord k => [a] -> (a -> k) -> [MGroup k a]
_group_by src keyfn =
  let go [] m order = (m, order)
      go (x:xs) m order =
        let k = keyfn x
        in case Map.lookup k m of
             Just is -> go xs (Map.insert k (is ++ [x]) m) order
             Nothing -> go xs (Map.insert k [x] m) (order ++ [k])
      (m, order) = go src Map.empty []
  in [ MGroup k (fromMaybe [] (Map.lookup k m)) | k <- order ]

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
  Just p | null p || p == "-" -> putStr text
         | otherwise -> writeFile p text

_split :: Char -> String -> [String]
_split _ "" = [""]
_split d s =
  let (h, t) = break (== d) s
  in h : case t of
            []      -> []
            (_:rest) -> _split d rest

_parseCSV :: String -> Bool -> Char -> [Map.Map String String]
_parseCSV text header delim =
  let ls = filter (not . null) (lines text)
  in if null ls then [] else
       let heads = if header
                      then _split delim (head ls)
                      else ["c" ++ show i | i <- [0 .. length (_split delim (head ls)) - 1]]
           start = if header then 1 else 0
           row line =
             let parts = _split delim line
             in Map.fromList [ (heads !! j, if j < length parts then parts !! j else "")
                             | j <- [0 .. length heads - 1] ]
      in map row (drop start ls)


data AnyValue = VInt Int | VDouble Double | VString String | VBool Bool deriving (Show)

_asInt :: AnyValue -> Int
_asInt (VInt n) = n
_asInt v = error ("expected int, got " ++ show v)

_asDouble :: AnyValue -> Double
_asDouble (VDouble d) = d
_asDouble v = error ("expected double, got " ++ show v)

_asString :: AnyValue -> String
_asString (VString s) = s
_asString v = error ("expected string, got " ++ show v)

_asBool :: AnyValue -> Bool
_asBool (VBool b) = b
_asBool v = error ("expected bool, got " ++ show v)

_showAny :: AnyValue -> String
_showAny (VInt n) = show n
_showAny (VDouble d) = show d
_showAny (VString s) = s
_showAny (VBool b) = if b then "true" else "false"


customers = [Map.fromList [("id", VInt (1)), ("name", VString ("Alice"))], Map.fromList [("id", VInt (2)), ("name", VString ("Bob"))], Map.fromList [("id", VInt (3)), ("name", VString ("Charlie"))], Map.fromList [("id", VInt (4)), ("name", VString ("Diana"))]]

orders = [Map.fromList [("id", 100), ("customerId", 1), ("total", 250)], Map.fromList [("id", 101), ("customerId", 2), ("total", 125)], Map.fromList [("id", 102), ("customerId", 1), ("total", 300)]]

result = [ Map.fromList [("customerName", fromMaybe (error "missing") (Map.lookup "name" c)), ("order", VString (o))] | o <- orders, c <- let _ms0 = [ c | c <- customers, (fromMaybe (error "missing") (Map.lookup "customerId" o) == _asInt (fromMaybe (error "missing") (Map.lookup "id" c))) ] in if null _ms0 then [Map.empty] else _ms0 ]

main :: IO ()
main = do
    putStrLn ("--- Right Join using syntax ---")
    mapM_ (\entry -> fromMaybe () (if fromMaybe (error "missing") (Map.lookup "order" entry) then (let _ = putStrLn (unwords ["Customer", _showAny (fromMaybe (error "missing") (Map.lookup "customerName" entry)), "has order", _showAny (fromMaybe (error "missing") (Map.lookup "id" (fromMaybe (error "missing") (Map.lookup "order" entry)))), "- $", _showAny (fromMaybe (error "missing") (Map.lookup "total" (fromMaybe (error "missing") (Map.lookup "order" entry))))]) in Nothing) else (let _ = putStrLn (unwords ["Customer", _showAny (fromMaybe (error "missing") (Map.lookup "customerName" entry)), "has no orders"]) in Nothing))) result
