matchAt :: String -> String -> Int -> Int -> Bool
matchAt s p i j
  | j >= length p = i >= length s
  | otherwise =
      let first = i < length s && (p !! j == '.' || s !! i == p !! j)
      in if j + 1 < length p && p !! (j + 1) == '*'
         then matchAt s p i (j + 2) || (first && matchAt s p (i + 1) j)
         else first && matchAt s p (i + 1) (j + 1)

solve :: String -> String
solve input = unlines (go t (drop 1 ls))
  where
    ls = lines input
    t = if null ls then 0 else read (head ls) :: Int
    go 0 _ = []
    go n (s:p:rest) = (if matchAt s p 0 0 then "true" else "false") : go (n - 1) rest
    go _ _ = []

main :: IO ()
main = interact (init . solve)
