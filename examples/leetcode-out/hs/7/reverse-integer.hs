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


reverse :: Int -> Int
reverse x = fromMaybe (0) $
    (let sign = 1 in (let n = x in case if (n < 0) then (let sign = (-1) in (let n = (-n) in Nothing)) else Nothing of Just v -> Just v; Nothing -> (let rev = 0 in (let rev = (rev * sign) in case if (((rev < (((-2147483647) - 1))) || rev) > 2147483647) then Just (0) else Nothing of Just v -> Just v; Nothing -> Just (rev)))))

main :: IO ()
main = do
    return ()
