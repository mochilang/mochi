value :: Char -> Int
value 'I' = 1
value 'V' = 5
value 'X' = 10
value 'L' = 50
value 'C' = 100
value 'D' = 500
value 'M' = 1000
value _ = 0

romanToInt :: String -> Int
romanToInt [] = 0
romanToInt [c] = value c
romanToInt (c:n:rest)
  | value c < value n = (- value c) + romanToInt (n:rest)
  | otherwise = value c + romanToInt (n:rest)

main :: IO ()
main = do
  contents <- getContents
  let vals = words contents
  case vals of
    [] -> pure ()
    (t:rest) -> mapM_ (print . romanToInt) (take (read t) rest)
