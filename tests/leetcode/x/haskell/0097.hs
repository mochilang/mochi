solve :: String -> String -> String -> Bool
solve s1 s2 s3
  | length s1 + length s2 /= length s3 = False
  | otherwise = dp !! length s1 !! length s2
  where
    m = length s1
    n = length s2
    dp = [[cell i j | j <- [0..n]] | i <- [0..m]]
    cell 0 0 = True
    cell i j = (i > 0 && dp !! (i - 1) !! j && s1 !! (i - 1) == s3 !! (i + j - 1))
            || (j > 0 && dp !! i !! (j - 1) && s2 !! (j - 1) == s3 !! (i + j - 1))

joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs

main :: IO ()
main = do
  ls <- lines <$> getContents
  case ls of
    [] -> pure ()
    tStr:rest -> do
      let t = read tStr :: Int
      putStr $ joinLines [if solve (rest !! (3*i)) (rest !! (3*i+1)) (rest !! (3*i+2)) then "true" else "false" | i <- [0..t-1]]
