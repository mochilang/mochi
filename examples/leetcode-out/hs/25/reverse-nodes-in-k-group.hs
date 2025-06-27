{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf)
import qualified Data.List as List
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL


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

avg :: Real a => [a] -> Double
avg xs | null xs = 0
      | otherwise = sum (map realToFrac xs) / fromIntegral (length xs)

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

_input :: IO String
_input = getLine

_now :: IO Int
_now = fmap round getPOSIXTime

_json :: Aeson.ToJSON a => a -> IO ()
_json v = BSL.putStrLn (Aeson.encode v)

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

_load :: Maybe String -> Maybe (Map.Map String String) -> IO [Map.Map String String]
_load path _ = do
  txt <- _readInput path
  pure (_parseCSV txt True ',')

_save :: [Map.Map String String] -> Maybe String -> Maybe (Map.Map String String) -> IO ()
_save rows path _ =
  let headers = if null rows then [] else Map.keys (head rows)
      toLine m = intercalate "," [Map.findWithDefault "" h m | h <- headers]
      text = unlines (if null headers then [] else intercalate "," headers : map toLine rows)
  in _writeOutput path text

expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"


reverseKGroup :: [Int] -> Int -> [Int]
reverseKGroup nums k = fromMaybe ([]) $
    (let n = length nums in case if (k <= 1) then Just (nums) else Nothing of Just v -> Just v; Nothing -> (let result = [] in (let i = 0 in case whileLoop (\() -> (i < n)) (\() -> (let end = (i + k) in case if (end <= n) then (let j = (end - 1) in whileLoop (\() -> (j >= i)) (\() -> (let result = (result + [(nums !! j)]) in (let j = (j - 1) in Nothing)))) else (let j = i in whileLoop (\() -> (j < n)) (\() -> (let result = (result + [(nums !! j)]) in (let j = (j + 1) in Nothing)))) of Just v -> Just v; Nothing -> (let i = (i + k) in Nothing))) of Just v -> Just v; Nothing -> Just (result))))
  where
    n = length nums

test_example_1 :: IO ()
test_example_1 = do
    expect ((reverseKGroup [1, 2, 3, 4, 5] 2 == [2, 1, 4, 3, 5]))

test_example_2 :: IO ()
test_example_2 = do
    expect ((reverseKGroup [1, 2, 3, 4, 5] 3 == [3, 2, 1, 4, 5]))

test_k_equals_list_length :: IO ()
test_k_equals_list_length = do
    expect ((reverseKGroup [1, 2, 3, 4] 4 == [4, 3, 2, 1]))

test_k_greater_than_length :: IO ()
test_k_greater_than_length = do
    expect ((reverseKGroup [1, 2, 3] 5 == [1, 2, 3]))

test_k_is_one :: IO ()
test_k_is_one = do
    expect ((reverseKGroup [1, 2, 3] 1 == [1, 2, 3]))

main :: IO ()
main = do
    test_example_1
    test_example_2
    test_k_equals_list_length
    test_k_greater_than_length
    test_k_is_one
