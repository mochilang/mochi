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


findSubstring :: String -> [String] -> [Int]
findSubstring s words = fromMaybe ([]) $
    case if (length words == 0) then Just ([]) else Nothing of Just v -> Just v; Nothing -> (let wordLen = length (words !! 0) in (let wordCount = length words in (let totalLen = (wordLen * wordCount) in case if (length s < totalLen) then Just ([]) else Nothing of Just v -> Just v; Nothing -> (let freq = Map.fromList [] in case foldr (\w acc -> case if (w in freq) then (let freq = ((freq !! w) + 1) in Nothing) else (let freq = 1 in Nothing) of Just v -> Just v; Nothing -> acc) Nothing words of Just v -> Just v; Nothing -> (let result = [] in case forLoop 0 wordLen (\offset -> (let left = offset in (let count = 0 in (let seen = Map.fromList [] in (let j = offset in Nothing))))) of Just v -> Just v; Nothing -> Just (result))))))

main :: IO ()
main = do
    return ()
