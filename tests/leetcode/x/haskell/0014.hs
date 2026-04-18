import Data.List (isPrefixOf)

lcp :: [String] -> String
lcp (x:xs) = go x
  where
    go p | all (isPrefixOf p) (x:xs) = p
         | otherwise = go (init p)

main :: IO ()
main = do
  c <- getContents
  let ts = words c
  case ts of
    [] -> pure ()
    (t:rest) -> solve (read t) rest
  where
    solve 0 _ = pure ()
    solve t (n:rest) = do
      let k = read n
      let strs = take k rest
      putStrLn ("\"" ++ lcp strs ++ "\"")
      solve (t - 1) (drop k rest)
