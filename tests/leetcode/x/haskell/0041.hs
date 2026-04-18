setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

place :: [Int] -> Int -> [Int]
place nums i
  | i >= n = nums
  | v >= 1 && v <= n && nums !! (v - 1) /= v = place swapped i
  | otherwise = place nums (i + 1)
  where
    n = length nums
    v = nums !! i
    target = nums !! (v - 1)
    swapped = setAt (v - 1) v (setAt i target nums)

firstMissingPositive :: [Int] -> Int
firstMissingPositive nums = scan placed 0
  where
    placed = place nums 0
    n = length nums
    scan xs i
      | i >= n = n + 1
      | xs !! i /= i + 1 = i + 1
      | otherwise = scan xs (i + 1)

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr :: Int) rest
  where
    go 0 _ = []
    go n xs =
      let m = read (head xs) :: Int
          vals = map read (take m (tail xs)) :: [Int]
          rems = drop (m + 1) xs
      in show (firstMissingPositive vals) : go (n - 1) rems

main :: IO ()
main = interact $ unlines . solveInput . lines
