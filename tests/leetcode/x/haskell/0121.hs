maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit (x:xs) = go xs x 0
  where
    go [] _ best = best
    go (p:ps) mn best = go ps (min mn p) (max best (p - mn))

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
      line = show (maxProfit vals)
      rems = drop n rest
   in if t == 1 then line else line ++ "\n" ++ solve (t - 1) rems
solve _ _ = ""
