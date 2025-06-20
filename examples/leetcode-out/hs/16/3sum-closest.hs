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


threeSumClosest :: [Int] -> Int -> Int
threeSumClosest nums target = fromMaybe (0) $
    (let sorted = 0 in (let n = length sorted in (let best = (((sorted !! 0) + (sorted !! 1)) + (sorted !! 2)) in case forLoop 0 n (\i -> (let left = (i + 1) in (let right = (n - 1) in Nothing))) of Just v -> Just v; Nothing -> Just (best))))
  where
    sorted = 0
    n = length sorted

main :: IO ()
main = do
    return ()
