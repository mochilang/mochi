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


longestCommonPrefix :: [String] -> String
longestCommonPrefix strs = fromMaybe ("") $
    case if (length strs == 0) then Just ("") else Nothing of Just v -> Just v; Nothing -> (let prefix = (strs !! 0) in case forLoop 1 length strs (\i -> (let j = 0 in (let current = (strs !! i) in (let prefix = (prefix !! 0) in if (prefix == "") then Nothing else Nothing)))) of Just v -> Just v; Nothing -> Just (prefix))

main :: IO ()
main = do
    return ()
