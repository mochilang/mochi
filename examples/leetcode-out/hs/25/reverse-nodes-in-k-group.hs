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


reverseKGroup :: [Int] -> Int -> [Int]
reverseKGroup nums k = fromMaybe ([]) $
    (let n = length nums in case if (k <= 1) then Just (nums) else Nothing of Just v -> Just v; Nothing -> (let result = [] in (let i = 0 in Just (result))))
  where
    n = length nums

main :: IO ()
main = do
    return ()
