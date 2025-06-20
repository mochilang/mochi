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


findMedianSortedArrays :: [Int] -> [Int] -> Double
findMedianSortedArrays nums1 nums2 = fromMaybe (0.0) $
    (let merged = [] in (let i = 0 in (let j = 0 in (let total = length merged in case if ((total `mod` 2) == 1) then Just ((merged !! (div total 2))) else Nothing of Just v -> Just v; Nothing -> (let mid1 = (merged !! ((div total 2) - 1)) in (let mid2 = (merged !! (div total 2)) in Just ((((mid1 + mid2)) / 2.0))))))))

main :: IO ()
main = do
    return ()
