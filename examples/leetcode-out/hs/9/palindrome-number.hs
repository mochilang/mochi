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


isPalindrome :: Int -> Bool
isPalindrome x = fromMaybe (False) $
    case if (x < 0) then Just (False) else Nothing of Just v -> Just v; Nothing -> (let s = show x in (let n = length s in case forLoop 0 (div n 2) (\i -> if ((s !! i) /= (s !! ((n - 1) - i))) then Just (False) else Nothing) of Just v -> Just v; Nothing -> Just (True)))

main :: IO ()
main = do
    return ()
