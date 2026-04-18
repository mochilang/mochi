isMatch :: String -> String -> Bool
isMatch s p = go 0 0 (-1) 0
  where
    go i j star mt
      | i < length s && j < length p && (p !! j == '?' || p !! j == s !! i) = go (i + 1) (j + 1) star mt
      | j < length p && p !! j == '*' = go i (j + 1) j i
      | star /= -1 && i < length s = go (mt + 1) (star + 1) star (mt + 1)
      | i >= length s = all (== '*') (drop j p)
      | otherwise = False

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr :: Int) rest
  where
    go 0 _ = []
    go n xs =
      let ns = read (head xs) :: Int
          xs1 = tail xs
          (s, xs2) = if ns > 0 then (head xs1, tail xs1) else ("", xs1)
          np = read (head xs2) :: Int
          xs3 = tail xs2
          (p, xs4) = if np > 0 then (head xs3, tail xs3) else ("", xs3)
      in (if isMatch s p then "true" else "false") : go (n - 1) xs4

main :: IO ()
main = interact $ unlines . solveInput . lines
