module Main where

twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target =
  case [(i,j) | i <- [0..n-1], j <- [i+1..n-1], nums!!i + nums!!j == target] of
    (p:_) -> p
    []    -> (-1,-1)
  where n = length nums

main :: IO ()
main = do
  let (i,j) = twoSum [2,7,11,15] 9
  print i
  print j
