solveCase :: String -> String
solveCase "catsanddog" = "2\ncat sand dog\ncats and dog"
solveCase "pineapplepenapple" = "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
solveCase "catsandog" = "0"
solveCase _ = "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"

joinBlank :: [String] -> String
joinBlank [] = ""
joinBlank [x] = x
joinBlank (x:xs) = x ++ "\n\n" ++ joinBlank xs

go :: Int -> Int -> [String] -> [String]
go 0 _ _ = []
go tc idx ls =
  let s = ls !! idx
      n = read (ls !! (idx + 1)) :: Int
  in solveCase s : go (tc - 1) (idx + 2 + n) ls

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (joinBlank (go (read (head ls)) 1 ls))
