intMin, intMax :: Int
intMin = -2147483648
intMax = 2147483647

reverseInt :: Int -> Int
reverseInt x = go x 0
  where
    go 0 ans = ans
    go n ans =
      let digit = n `rem` 10
          n' = n `quot` 10
       in if ans > intMax `quot` 10 || (ans == intMax `quot` 10 && digit > 7)
            then 0
            else if ans < intMin `quot` 10 || (ans == intMin `quot` 10 && digit < -8)
              then 0
              else go n' (ans * 10 + digit)

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else do
    let t = read (head ls) :: Int
    putStr (unlinesWith (map (show . reverseInt . read) (take t (tail ls))))

unlinesWith :: [String] -> String
unlinesWith [] = ""
unlinesWith [x] = x
unlinesWith (x:xs) = x ++ "\n" ++ unlinesWith xs
