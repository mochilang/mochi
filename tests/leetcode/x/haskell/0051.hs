solve :: Int -> [[String]]
solve n = dfs 0 cols d1 d2 []
  where
    cols = replicate n False
    d1 = replicate (2 * n) False
    d2 = replicate (2 * n) False
    set xs i v = take i xs ++ [v] ++ drop (i + 1) xs
    dfs r cols' d1' d2' board
      | r == n = [reverse board]
      | otherwise = concat [ let a = r + c; b = r + (n - 1 - c); row = replicate c '.' ++ "Q" ++ replicate (n - c - 1) '.' in if cols' !! c || d1' !! a || d2' !! b then [] else dfs (r + 1) (set cols' c True) (set d1' a True) (set d2' b True) (row : board) | c <- [0..n-1]]

renderBlock :: [[String]] -> [String]
renderBlock [] = ["0"]
renderBlock sols = show (length sols) : concat [sol ++ if i + 1 < length sols then ["-"] else [] | (i, sol) <- zip [0..] sols]

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr :: Int) rest 0
  where
    go 0 _ _ = []
    go t xs k = let n = read (head xs) :: Int; block = renderBlock (solve n); rest' = go (t - 1) (tail xs) (k + 1) in block ++ (if t > 1 then ["="] else []) ++ rest'

main :: IO ()
main = interact $ unlines . solveInput . lines
