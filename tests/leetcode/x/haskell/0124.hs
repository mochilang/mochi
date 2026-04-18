import Data.Array

solve :: Array Int Int -> Array Int Bool -> Int
solve vals ok = snd (dfs 0)
  where
    (_, r) = bounds vals
    dfs i
      | i > r = (0, -1000000000)
      | not (ok ! i) = (0, -1000000000)
      | otherwise =
          let (lg0, lb) = dfs (2 * i + 1)
              (rg0, rb) = dfs (2 * i + 2)
              lg = max 0 lg0
              rg = max 0 rg0
              v = vals ! i
              best = maximum [lb, rb, v + lg + rg]
           in (v + max lg rg, best)

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else do
    let tc = read (head ls) :: Int
    putStr (go tc 1 ls)

parseTok :: String -> (Int, Bool)
parseTok "null" = (0, False)
parseTok s = (read s, True)

go :: Int -> Int -> [String] -> String
go 0 _ _ = ""
go tc idx ls =
  let n = read (ls !! idx) :: Int
      toks = take n (drop (idx + 1) ls)
      parsed = map parseTok toks
      vals = listArray (0, n - 1) (map fst parsed)
      ok = listArray (0, n - 1) (map snd parsed)
      line = show (solve vals ok)
      rest = go (tc - 1) (idx + 1 + n) ls
   in if tc == 1 then line else line ++ "\n" ++ rest
