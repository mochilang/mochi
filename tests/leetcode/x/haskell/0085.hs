hist :: [Int] -> Int
hist h = maximum [minimum (take (j - i + 1) (drop i h)) * (j - i + 1) | i <- [0 .. n - 1], j <- [i .. n - 1]]
  where n = length h

solveCase :: [[Char]] -> Int
solveCase rows = go rows (replicate (length (head rows)) 0) 0
  where
    go [] _ best = best
    go (s:ss) h best =
      let h2 = zipWith (\v ch -> if ch == '1' then v + 1 else 0) h s
      in go ss h2 (max best (hist h2))

parseCases :: Int -> [String] -> [String]
parseCases 0 _ = []
parseCases t (r:c:xs) =
  let rows = read r :: Int
      cols = read c :: Int
      block = take rows xs
      _ = cols
  in show (solveCase block) : parseCases (t - 1) (drop rows xs)
parseCases _ _ = []

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs

main :: IO ()
main = do
  s <- getContents
  let ls = words s
  case ls of
    [] -> pure ()
    tStr:rest -> putStr (joinLines (parseCases (read tStr) rest))
