module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map


forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i | i < end =
            case f i of
              Just v -> Just v
              Nothing -> go (i + 1)
         | otherwise = Nothing

avg :: Real a => [a] -> Double
avg xs | null xs = 0
      | otherwise = sum (map realToFrac xs) / fromIntegral (length xs)

_indexString :: String -> Int -> String
_indexString s i =
  let idx = if i < 0 then i + length s else i
  in if idx < 0 || idx >= length s
       then error "index out of range"
       else [s !! idx]

_input :: IO String
_input = getLine


letterCombinations :: String -> [String]
letterCombinations digits = fromMaybe ([]) $
    case if (length digits == 0) then Just ([]) else Nothing of Just v -> Just v; Nothing -> (let mapping = Map.fromList [("2", ["a", "b", "c"]), ("3", ["d", "e", "f"]), ("4", ["g", "h", "i"]), ("5", ["j", "k", "l"]), ("6", ["m", "n", "o"]), ("7", ["p", "q", "r", "s"]), ("8", ["t", "u", "v"]), ("9", ["w", "x", "y", "z"])] in (let result = [""] in case foldr (\d acc -> case case if not ((d in mapping)) then Nothing else Nothing of Just v -> Just v; Nothing -> (let letters = (mapping !! d) in (let next = 0 in (let result = next in Nothing))) of Just v -> Just v; Nothing -> acc) Nothing digits of Just v -> Just v; Nothing -> Just (result)))

main :: IO ()
main = do
    return ()
