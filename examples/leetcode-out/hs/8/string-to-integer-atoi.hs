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


myAtoi s = fromMaybe (0) $
    (let i = 0 in (let n = length s in (let sign = 1 in case if ((i < n) && (((((s !! i) == "+") || (s !! i)) == "-"))) then case if ((s !! i) == "-") then (let sign = (-1) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let i = (i + 1) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let digits = Map.fromList [("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)] in (let result = 0 in (let result = (result * sign) in case if (result > 2147483647) then Just (2147483647) else Nothing of Just v -> Just v; Nothing -> case if (result < ((-2147483648))) then Just ((-2147483648)) else Nothing of Just v -> Just v; Nothing -> Just (result)))))))

main :: IO ()
main = do
    return ()
