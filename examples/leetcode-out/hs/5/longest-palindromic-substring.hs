module Main where

import Data.Maybe (fromMaybe)


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


expand s left right = fromMaybe (0) $
    (let l = left in (let r = right in (let n = length s in Just (((r - l) - 1)))))

longestPalindrome s = fromMaybe ("") $
    case if (length s <= 1) then Just (s) else Nothing of Just v -> Just v; Nothing -> (let start = 0 in (let end = 0 in (let n = length s in case forLoop 0 n (\i -> (let len1 = expand s i i in (let len2 = expand s i (i + 1) in (let l = len1 in case if (len2 > len1) then (let l = len2 in Nothing) else Nothing of Just v -> Just v; Nothing -> if ((l > end) - start) then (let start = (div (i - ((l - 1))) 2) in (let end = (div (i + l) 2) in Nothing)) else Nothing)))) of Just v -> Just v; Nothing -> Just ((s !! start)))))

main :: IO ()
main = do
    return ()
