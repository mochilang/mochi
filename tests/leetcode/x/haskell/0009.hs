reverseDigits :: Int -> Int -> Int
reverseDigits 0 rev = rev
reverseDigits x rev = reverseDigits (x `div` 10) (rev * 10 + x `mod` 10)

isPalindrome :: Int -> Bool
isPalindrome x | x < 0 = False
isPalindrome x = x == reverseDigits x 0

main :: IO ()
main = do
  contents <- getContents
  let vals = map read (words contents) :: [Int]
  case vals of
    [] -> pure ()
    (t:rest) -> mapM_ (\x -> putStrLn (if isPalindrome x then "true" else "false")) (take t rest)
