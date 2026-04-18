set :: [Bool] -> Int -> Bool -> [Bool]
set xs i v = take i xs ++ [v] ++ drop (i + 1) xs

countSolutions :: Int -> Int -> [Bool] -> [Bool] -> [Bool] -> Int
countSolutions r n cols d1 d2
  | r == n = 1
  | otherwise = sum [countSolutions (r + 1) n (set cols c True) (set d1 a True) (set d2 b True) | c <- [0..n-1], let a = r + c, let b = r + (n - 1 - c), not (cols !! c || d1 !! a || d2 !! b)]

solve :: Int -> Int
solve n = countSolutions 0 n (replicate n False) (replicate (2 * n) False) (replicate (2 * n) False)

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr :: Int) rest
  where
    go 0 _ = []
    go t (x:xs) = let n = read x :: Int; block = [show (solve n)]; rest' = go (t - 1) xs in block ++ rest'
    go _ [] = []

main :: IO ()
main = interact $ unlines . solveInput . lines
