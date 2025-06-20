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


isMatch :: String -> String -> Bool
isMatch s p = fromMaybe (False) $
    (let m = length s in (let n = length p in (let dp = [] in (let i = 0 in (let dp = True in (let i2 = m in Just (((dp !! 0) !! 0))))))))
  where
    m = length s
    n = length p

main :: IO ()
main = do
    return ()
