maxArea :: [Int] -> Int
maxArea h = go 0 (length h - 1) 0
  where
    go l r best
      | l >= r = best
      | otherwise =
          let height = min (h !! l) (h !! r)
              best' = max best ((r - l) * height)
           in if h !! l < h !! r then go (l + 1) r best' else go l (r - 1) best'

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else do
    let t = read (head ls) :: Int
    putStr (solve t (tail ls))

solve :: Int -> [String] -> String
solve 0 _ = ""
solve t (nStr:rest) =
  let n = read nStr :: Int
      vals = map read (take n rest) :: [Int]
      line = show (maxArea vals)
      rems = drop n rest
   in if t == 1 then line else line ++ "\n" ++ solve (t - 1) rems
solve _ _ = ""
