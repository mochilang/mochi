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


convert :: String -> Int -> String
convert s numRows = fromMaybe ("") $
    case if (((numRows <= 1) || numRows) >= length s) then Just (s) else Nothing of Just v -> Just v; Nothing -> (let rows = [] in (let i = 0 in (let curr = 0 in (let step = 1 in case foldr (\ch acc -> case (let rows = ((rows !! curr) + ch) in case if (curr == 0) then (let step = 1 in Nothing) else if ((curr == numRows) - 1) then (let step = (-1) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let curr = (curr + step) in Nothing)) of Just v -> Just v; Nothing -> acc) Nothing s of Just v -> Just v; Nothing -> (let result = "" in case foldr (\row acc -> case (let result = (result + row) in Nothing) of Just v -> Just v; Nothing -> acc) Nothing rows of Just v -> Just v; Nothing -> Just (result))))))

main :: IO ()
main = do
    return ()
