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


divide :: Int -> Int -> Int
divide dividend divisor = fromMaybe (0) $
    case if (((dividend == (((-2147483647) - 1))) && divisor) == ((-1))) then Just (2147483647) else Nothing of Just v -> Just v; Nothing -> (let negative = False in case if (dividend < 0) then (let negative = not negative in (let dividend = (-dividend) in Nothing)) else Nothing of Just v -> Just v; Nothing -> case if (divisor < 0) then (let negative = not negative in (let divisor = (-divisor) in Nothing)) else Nothing of Just v -> Just v; Nothing -> (let quotient = 0 in case if negative then (let quotient = (-quotient) in Nothing) else Nothing of Just v -> Just v; Nothing -> case if (quotient > 2147483647) then Just (2147483647) else Nothing of Just v -> Just v; Nothing -> case if (quotient < (((-2147483647) - 1))) then Just ((-2147483648)) else Nothing of Just v -> Just v; Nothing -> Just (quotient)))

main :: IO ()
main = do
    return ()
