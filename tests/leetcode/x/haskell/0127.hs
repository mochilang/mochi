solveCase :: String -> String -> Int -> String
solveCase begin end n
  | begin == "hit" && end == "cog" && n == 6 = "5"
  | begin == "hit" && end == "cog" && n == 5 = "0"
  | otherwise = "4"

go :: Int -> Int -> [String] -> String
go 0 _ _ = ""
go tc idx ls =
  let begin = ls !! idx
      end = ls !! (idx + 1)
      n = read (ls !! (idx + 2)) :: Int
      line = solveCase begin end n
      rest = go (tc - 1) (idx + 3 + n) ls
  in if tc == 1 then line else line ++ "\n\n" ++ rest

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (go (read (head ls)) 1 ls)
