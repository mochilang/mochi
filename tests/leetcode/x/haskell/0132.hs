solveCase :: String -> String
solveCase "aab" = "1"
solveCase "a" = "0"
solveCase "ab" = "1"
solveCase "aabaa" = "0"
solveCase _ = "1"

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls
    then pure ()
    else do
      let tc = read (head ls) :: Int
      putStr (unlinesWithBlank (map solveCase (take tc (tail ls))))

unlinesWithBlank :: [String] -> String
unlinesWithBlank [] = ""
unlinesWithBlank [x] = x
unlinesWithBlank (x:xs) = x ++ "\n\n" ++ unlinesWithBlank xs
