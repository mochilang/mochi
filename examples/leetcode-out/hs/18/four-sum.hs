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


fourSum :: [Int] -> Int -> [[Int]]
fourSum nums target = fromMaybe ([]) $
    (let sorted = 0 in (let n = length sorted in (let result = [] in case forLoop 0 n (\i -> case if (((i > 0) && (sorted !! i)) == (sorted !! (i - 1))) then Nothing else Nothing of Just v -> Just v; Nothing -> forLoop (i + 1) n (\j -> case if ((((j > i) + 1) && (sorted !! j)) == (sorted !! (j - 1))) then Nothing else Nothing of Just v -> Just v; Nothing -> (let left = (j + 1) in (let right = (n - 1) in Nothing)))) of Just v -> Just v; Nothing -> Just (result))))
  where
    sorted = 0
    n = length sorted

main :: IO ()
main = do
    return ()
