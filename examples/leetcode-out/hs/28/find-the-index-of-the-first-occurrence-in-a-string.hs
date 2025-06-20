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


strStr :: String -> String -> Int
strStr haystack needle = fromMaybe (0) $
    (let n = length haystack in (let m = length needle in case if (m == 0) then Just (0) else Nothing of Just v -> Just v; Nothing -> case if (m > n) then Just ((-1)) else Nothing of Just v -> Just v; Nothing -> case forLoop 0 ((n - m) + 1) (\i -> (let j = 0 in if (j == m) then Just (i) else Nothing)) of Just v -> Just v; Nothing -> Just ((-1))))
  where
    n = length haystack
    m = length needle

main :: IO ()
main = do
    return ()
