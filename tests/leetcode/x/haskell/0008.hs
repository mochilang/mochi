myAtoi :: String -> Int
myAtoi s = go rest 0
  where
    trimmed = dropWhile (== ' ') s
    (sign, rest) = case trimmed of
      ('-':xs) -> (-1, xs)
      ('+':xs) -> (1, xs)
      _ -> (1, trimmed)
    limit = if sign == 1 then 7 else 8
    go (c:cs) ans
      | c >= '0' && c <= '9' =
          let digit = fromEnum c - fromEnum '0'
           in if ans > 214748364 || (ans == 214748364 && digit > limit)
                then if sign == 1 then 2147483647 else -2147483648
                else go cs (ans * 10 + digit)
    go _ ans = sign * ans

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else do
    let t = read (head ls) :: Int
        out = map (show . myAtoi) (take t (tail ls ++ repeat ""))
    putStr (joinLines out)

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs
