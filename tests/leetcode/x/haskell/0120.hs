solve :: [[Int]] -> Int
solve tri = head $ foldr1 step tri where step row dp = zipWith (+) row (zipWith min dp (tail dp))

joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs

main :: IO ()
main = do
  let parse [] = []
      parse (rows:xs) = let r = read rows :: Int; (tri, rest) = go 1 r xs [] in solve tri : parse rest
      go i r xs acc | i > r = (reverse acc, xs)
      go i r xs acc = let row = map read (take i xs) in go (i + 1) r (drop i xs) (row : acc)
  ws <- words <$> getContents
  case ws of
    [] -> pure ()
    tStr:rest -> putStr $ joinLines $ map show $ take (read tStr) (parse rest)
