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


twoSum :: [Int] -> Int -> [Int]
twoSum nums target = fromMaybe ([]) $
    (let n = length nums in case forLoop 0 n (\i -> forLoop (i + 1) n (\j -> if (((nums !! i) + (nums !! j)) == target) then Just ([i, j]) else Nothing)) of Just v -> Just v; Nothing -> Just ([(-1), (-1)]))
  where
    n = length nums

main :: IO ()
main = do
    let result = twoSum [2, 7, 11, 15] 9
    print ((result !! 0))
    print ((result !! 1))
