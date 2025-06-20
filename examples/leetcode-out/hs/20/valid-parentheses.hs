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


isValid :: String -> Bool
isValid s = fromMaybe (False) $
    (let stack = [] in (let n = length s in case forLoop 0 n (\i -> (let c = (s !! i) in if (c == "(") then (let stack = (stack + [")"]) in Nothing) else if (c == "[") then (let stack = (stack + ["]"]) in Nothing) else if (c == "{") then (let stack = (stack + ["}"]) in Nothing) else case if (length stack == 0) then Just (False) else Nothing of Just v -> Just v; Nothing -> (let top = (stack !! (length stack - 1)) in case if (top /= c) then Just (False) else Nothing of Just v -> Just v; Nothing -> (let stack = (stack !! 0) in Nothing)))) of Just v -> Just v; Nothing -> Just ((length stack == 0))))

main :: IO ()
main = do
    return ()
