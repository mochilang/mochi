-- Generated by Mochi compiler v0.10.26 on 2025-07-16T09:31:12Z
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

poly :: Int -> String
poly p =
  fromMaybe ("") $
    (let s = "" in (let coef = 1 in (let i = p in case if (_asInt (coef) /= 1) then (let s = (s + show coef) in Nothing) else Nothing of Just v -> Just v; Nothing -> case whileLoop (\() -> (_asInt (i) > 0)) (\() -> (let s = (s + "x") in case if (_asInt (i) /= 1) then (let s = ((s + "^") + show i) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let coef = (read (((coef * i) / ((_asInt ((p - i)) + 1)))) :: Int) in (let d = coef in case if ((((p - ((_asInt (i) - 1)))) `mod` 2) == 1) then (let d = (-(_asInt (d))) in Nothing) else Nothing of Just v -> Just v; Nothing -> case if (_asInt (d) < 0) then (let s = ((s + " - ") + show (-(_asInt (d)))) in Nothing) else (let s = ((s + " + ") + show d) in Nothing) of Just v -> Just v; Nothing -> (let i = (_asInt (i) - 1) in Nothing))))) of Just v -> Just v; Nothing -> case if (s == "") then (let s = "1" in Nothing) else Nothing of Just v -> Just v; Nothing -> Just (s))))

aks :: Int -> Bool
aks n = fromMaybe (False) $
  case if (_asInt (n) < 2) then Just (False) else Nothing of Just v -> Just v; Nothing -> (let c = n in (let i = 1 in case whileLoop (\() -> (i < n)) (\() -> case if ((c `mod` n) /= 0) then Just (False) else Nothing of Just v -> Just v; Nothing -> (let c = (read (((c * ((n - i))) / ((_asInt (i) + 1)))) :: Int) in (let i = (_asInt (i) + 1) in Nothing))) of Just v -> Just v; Nothing -> Just (True)))

main :: ()
main =
  fromMaybe (()) $
    (let p = 0 in case whileLoop (\() -> (_asInt (p) <= 7)) (\() -> case (let _ = putStrLn (((show p ++ ":  ") ++ poly p)) in Nothing) of Just v -> Just v; Nothing -> (let p = (_asInt (p) + 1) in Nothing)) of Just v -> Just v; Nothing -> (let first = True in (let p = 2 in (let line = "" in case whileLoop (\() -> (_asInt (p) < 50)) (\() -> case if aks p then if first then (let line = (line + show p) in (let first = False in Nothing)) else (let line = ((line + " ") + show p) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let p = (_asInt (p) + 1) in Nothing)) of Just v -> Just v; Nothing -> (let _ = putStrLn (_showAny (line)) in Nothing)))))

main :: IO ()
main = do
  main
