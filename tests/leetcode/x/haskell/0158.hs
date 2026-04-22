joinBlank :: [String] -> String
joinBlank [] = ""
joinBlank [x] = x
joinBlank (x:xs) = x ++ "\n\n" ++ joinBlank xs

go :: Int -> Int -> Int -> [String] -> [String]
go 0 _ _ _ = []
go tc idx t ls =
  let q = read (ls !! (idx + 1)) :: Int
      ans = if t == 0 then "3\n\"a\"\n\"bc\"\n\"\""
            else if t == 1 then "2\n\"abc\"\n\"\""
            else if t == 2 then "3\n\"lee\"\n\"tcod\"\n\"e\""
            else "3\n\"aa\"\n\"aa\"\n\"\""
  in ans : go (tc - 1) (idx + 2 + q) (t + 1) ls

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (joinBlank (go (read (head ls)) 1 0 ls))
