setAt :: Int -> Int -> [Int] -> [Int]
setAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs

solve :: String -> String -> Int
solve s t = foldl step base s !! length t
  where
    base = 1 : replicate (length t) 0
    step dp ch = foldr upd dp [1 .. length t]
      where
        upd j cur = if ch == t !! (j - 1)
                    then setAt j (cur !! j + cur !! (j - 1)) cur
                    else cur

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else do
    let tc = read (head ls) :: Int
    let ans = [show (solve (ls !! (1 + 2 * i)) (ls !! (2 + 2 * i))) | i <- [0 .. tc - 1]]
    putStr (unlines (take (length ans - 1) ans ++ [last ans]))
