import Data.List (foldl')

expandAt :: String -> Int -> Int -> (Int, Int)
expandAt s left right
  | left >= 0 && right < length s && s !! left == s !! right = expandAt s (left - 1) (right + 1)
  | otherwise = (left + 1, right - left - 1)

longestPalindrome :: String -> String
longestPalindrome s = take bestLen (drop bestStart s)
  where
    initial = (0, if null s then 0 else 1)
    (bestStart, bestLen) = foldl' step initial [0 .. length s - 1]
    step (bs, bl) i =
      let (s1, l1) = expandAt s i i
          (bs1, bl1) = if l1 > bl then (s1, l1) else (bs, bl)
          (s2, l2) = expandAt s i (i + 1)
       in if l2 > bl1 then (s2, l2) else (bs1, bl1)

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls
    then pure ()
    else do
      let t = read (head ls) :: Int
          ss = take t (drop 1 ls ++ repeat "")
      putStr (unlinesWith (map longestPalindrome ss))

unlinesWith :: [String] -> String
unlinesWith [] = ""
unlinesWith [x] = x
unlinesWith (x:xs) = x ++ "\n" ++ unlinesWith xs
