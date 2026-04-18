trap :: [Int] -> Int
trap h = go 0 (length h - 1) 0 0 0
  where
    go left right leftMax rightMax water
      | left > right = water
      | leftMax <= rightMax =
          let v = h !! left
          in if v < leftMax then go (left + 1) right leftMax rightMax (water + leftMax - v) else go (left + 1) right v rightMax water
      | otherwise =
          let v = h !! right
          in if v < rightMax then go left (right - 1) leftMax rightMax (water + rightMax - v) else go left (right - 1) leftMax v water

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr :: Int) rest
  where
    go 0 _ = []
    go n xs =
      let m = read (head xs) :: Int
          vals = map read (take m (tail xs)) :: [Int]
          rems = drop (m + 1) xs
      in show (trap vals) : go (n - 1) rems

main :: IO ()
main = interact $ unlines . solveInput . lines
