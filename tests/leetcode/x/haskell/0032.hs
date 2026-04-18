solveCase :: String -> Int
solveCase s = go s 0 [-1] 0 where
  go [] _ _ best = best
  go (c:cs) i stack best | c == '(' = go cs (i+1) (i:stack) best
                         | otherwise = case stack of
                             (_:rest) -> if null rest then go cs (i+1) [i] best else go cs (i+1) rest (max best (i - head rest))
                             [] -> go cs (i+1) [i] best
main = interact $ unlines . solve . lines where
  solve [] = []
  solve (tStr:rest) = go (read tStr :: Int) rest
  go 0 _ = []
  go n (nStr:xs) = let m = read nStr :: Int; (s,rems) = if m > 0 then (head xs, tail xs) else ("", xs) in show (solveCase s) : go (n-1) rems
  go _ _ = []
