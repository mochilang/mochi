maxProfit :: [Int] -> Int
maxProfit = go (-1000000000) 0 (-1000000000) 0
  where
    go _ _ _ sell2 [] = sell2
    go buy1 sell1 buy2 sell2 (p:ps) =
      let buy1' = max buy1 (-p)
          sell1' = max sell1 (buy1' + p)
          buy2' = max buy2 (sell1' - p)
          sell2' = max sell2 (buy2' + p)
       in go buy1' sell1' buy2' sell2' ps

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
