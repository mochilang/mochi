-- Generated by Mochi compiler v0.10.26 on 2025-07-16T09:30:30Z
-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.List as List
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

trimSpace :: String -> String
trimSpace s =
  fromMaybe ("") $
    (let start = 0 in case whileLoop (\() -> (((_asInt (start) < length s) && take ((_asInt (start) + 1) - start) (drop start s)) == " ")) (\() -> (let start = (_asInt (start) + 1) in Nothing)) of Just v -> Just v; Nothing -> (let end = length s in case whileLoop (\() -> (((end > start) && take (end - (_asInt (end) - 1)) (drop (_asInt (end) - 1) s)) == " ")) (\() -> (let end = (_asInt (end) - 1) in Nothing)) of Just v -> Just v; Nothing -> Just (take (end - start) (drop start s))))

isUpper :: String -> Bool
isUpper ch = (((ch >= "A") && ch) <= "Z")

padLeft :: String -> Int -> String
padLeft s w =
  fromMaybe ("") $
    (let res = "" in (let n = (_asInt (w) - length s) in case whileLoop (\() -> (_asInt (n) > 0)) (\() -> (let res = (res + " ") in (let n = (_asInt (n) - 1) in Nothing))) of Just v -> Just v; Nothing -> Just ((res + s))))

snakeToCamel :: String -> String
snakeToCamel s =
  fromMaybe ("") $
    (let s = trimSpace s in (let out = "" in (let up = False in (let i = 0 in case whileLoop (\() -> (_asInt (i) < length s)) (\() -> (let ch = take ((_asInt (i) + 1) - i) (drop i s) in case if (((((((ch == "_") || ch) == "-") || ch) == " ") || ch) == ".") then (let up = True in (let i = (_asInt (i) + 1) in Nothing)) else Nothing of Just v -> Just v; Nothing -> case if (_asInt (i) == 0) then (let out = (out + lower ch) in (let up = False in (let i = (_asInt (i) + 1) in Nothing))) else Nothing of Just v -> Just v; Nothing -> case if _asBool (up) then (let out = (out + upper ch) in (let up = False in Nothing)) else (let out = (out + ch) in Nothing) of Just v -> Just v; Nothing -> (let i = (_asInt (i) + 1) in Nothing))) of Just v -> Just v; Nothing -> Just (out)))))

camelToSnake :: String -> String
camelToSnake s =
  fromMaybe ("") $
    (let s = trimSpace s in (let out = "" in (let prevUnd = False in (let i = 0 in case whileLoop (\() -> (_asInt (i) < length s)) (\() -> (let ch = take ((_asInt (i) + 1) - i) (drop i s) in case if (((((ch == " ") || ch) == "-") || ch) == ".") then case if _asBool ((_asInt ((not prevUnd && length out)) > 0)) then (let out = (out + "_") in (let prevUnd = True in Nothing)) else Nothing of Just v -> Just v; Nothing -> (let i = (_asInt (i) + 1) in Nothing) else Nothing of Just v -> Just v; Nothing -> case if (ch == "_") then case if _asBool ((_asInt ((not prevUnd && length out)) > 0)) then (let out = (out + "_") in (let prevUnd = True in Nothing)) else Nothing of Just v -> Just v; Nothing -> (let i = (_asInt (i) + 1) in Nothing) else Nothing of Just v -> Just v; Nothing -> case if isUpper ch then case if _asBool (((_asInt (i) > 0) && (not prevUnd))) then (let out = (out + "_") in Nothing) else Nothing of Just v -> Just v; Nothing -> (let out = (out + lower ch) in (let prevUnd = False in Nothing)) else (let out = (out + lower ch) in (let prevUnd = False in Nothing)) of Just v -> Just v; Nothing -> (let i = (_asInt (i) + 1) in Nothing))) of Just v -> Just v; Nothing -> (let start = 0 in case whileLoop (\() -> (((_asInt (start) < length out) && take ((_asInt (start) + 1) - start) (drop start out)) == "_")) (\() -> (let start = (_asInt (start) + 1) in Nothing)) of Just v -> Just v; Nothing -> (let end = length out in case whileLoop (\() -> (((end > start) && take (end - (_asInt (end) - 1)) (drop (_asInt (end) - 1) out)) == "_")) (\() -> (let end = (_asInt (end) - 1) in Nothing)) of Just v -> Just v; Nothing -> (let out = take (end - start) (drop start out) in (let res = "" in (let j = 0 in (let lastUnd = False in case whileLoop (\() -> (_asInt (j) < length out)) (\() -> (let c = take ((_asInt (j) + 1) - j) (drop j out) in case if (c == "_") then case if _asBool (not lastUnd) then (let res = (res + c) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let lastUnd = True in Nothing) else (let res = (res + c) in (let lastUnd = False in Nothing)) of Just v -> Just v; Nothing -> (let j = (_asInt (j) + 1) in Nothing))) of Just v -> Just v; Nothing -> Just (res)))))))))))

main :: ()
main =
  fromMaybe (()) $
    (let samples = ["snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces "] in case (let _ = putStrLn ("=== To snake_case ===") in Nothing) of Just v -> Just v; Nothing -> case foldr (\s acc -> case (let _ = putStrLn (((padLeft s 34 ++ " => ") ++ camelToSnake s)) in Nothing) of Just v -> Just v; Nothing -> acc) Nothing samples of Just v -> Just v; Nothing -> case (let _ = putStrLn ("") in Nothing) of Just v -> Just v; Nothing -> case (let _ = putStrLn ("=== To camelCase ===") in Nothing) of Just v -> Just v; Nothing -> foldr (\s acc -> case (let _ = putStrLn (((padLeft s 34 ++ " => ") ++ snakeToCamel s)) in Nothing) of Just v -> Just v; Nothing -> acc) Nothing samples)
  where
    samples = ["snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces "]

main :: IO ()
main = do
  main
