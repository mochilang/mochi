match :: Char -> Char -> Bool
match ')' '(' = True
match ']' '[' = True
match '}' '{' = True
match _ _ = False

isValid :: String -> Bool
isValid = go []
  where
    go stack [] = null stack
    go stack (c:cs)
      | c `elem` "([{" = go (c:stack) cs
      | otherwise = case stack of
          open:rest | match c open -> go rest cs
          _ -> False

main :: IO ()
main = do
  contents <- getContents
  let vals = words contents
  case vals of
    [] -> pure ()
    (t:rest) -> mapM_ (putStrLn . boolString . isValid) (take (read t) rest)
  where
    boolString True = "true"
    boolString False = "false"
