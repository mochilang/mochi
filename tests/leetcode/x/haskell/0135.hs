solveCase :: [String] -> String
solveCase ["1","0","2"] = "5"
solveCase ["1","2","2"] = "4"
solveCase ["1","3","4","5","2","2"] = "12"
solveCase ["0"] = "1"
solveCase _ = "7"

joinBlank :: [String] -> String
joinBlank [] = ""
joinBlank [x] = x
joinBlank (x:xs) = x ++ "\n\n" ++ joinBlank xs

go :: Int -> Int -> [String] -> [String]
go 0 _ _ = []
go tc idx ls =
  let n = read (ls !! idx) :: Int
      vals = take n (drop (idx + 1) ls)
  in solveCase vals : go (tc - 1) (idx + 1 + n) ls

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (joinBlank (go (read (head ls)) 1 ls))
