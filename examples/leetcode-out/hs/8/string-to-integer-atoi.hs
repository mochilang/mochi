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


digit :: String -> Int
digit ch = fromMaybe (0) $
    case if (ch == "0") then Just (0) else Nothing of Just v -> Just v; Nothing -> case if (ch == "1") then Just (1) else Nothing of Just v -> Just v; Nothing -> case if (ch == "2") then Just (2) else Nothing of Just v -> Just v; Nothing -> case if (ch == "3") then Just (3) else Nothing of Just v -> Just v; Nothing -> case if (ch == "4") then Just (4) else Nothing of Just v -> Just v; Nothing -> case if (ch == "5") then Just (5) else Nothing of Just v -> Just v; Nothing -> case if (ch == "6") then Just (6) else Nothing of Just v -> Just v; Nothing -> case if (ch == "7") then Just (7) else Nothing of Just v -> Just v; Nothing -> case if (ch == "8") then Just (8) else Nothing of Just v -> Just v; Nothing -> case if (ch == "9") then Just (9) else Nothing of Just v -> Just v; Nothing -> Just ((-1))

myAtoi :: String -> Int
myAtoi s = fromMaybe (0) $
    (let i = 0 in (let n = length s in (let sign = 1 in case if ((i < n) && (((((s !! i) == _indexString "+" 0) || (s !! i)) == _indexString "-" 0))) then case if ((s !! i) == _indexString "-" 0) then (let sign = (-1) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let i = (i + 1) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let result = 0 in (let result = (result * sign) in case if (result > 2147483647) then Just (2147483647) else Nothing of Just v -> Just v; Nothing -> case if (result < ((-2147483648))) then Just ((-2147483648)) else Nothing of Just v -> Just v; Nothing -> Just (result))))))

main :: IO ()
main = do
    return ()
