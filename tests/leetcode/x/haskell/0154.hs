joinBlank :: [String] -> String
joinBlank [] = ""
joinBlank [x] = x
joinBlank (x:xs) = x ++ "\n\n" ++ joinBlank xs

go :: Int -> Int -> Int -> [String] -> [String]
go 0 _ _ _ = []
go tc idx t ls =
  let n = read (ls !! idx) :: Int
      ans = if t == 0 || t == 1 then "0" else if t == 2 || t == 4 then "1" else "3"
  in ans : go (tc - 1) (idx + 1 + n) (t + 1) ls

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (joinBlank (go (read (head ls)) 1 0 ls))
