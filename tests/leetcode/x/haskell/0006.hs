convertZigzag :: String -> Int -> String
convertZigzag s numRows
  | numRows <= 1 || numRows >= length s = s
  | otherwise = concatMap rowChars [0 .. numRows - 1]
  where
    cycleLen = 2 * numRows - 2
    rowChars row = concatMap charsAt [row, row + cycleLen .. length s - 1]
      where
        charsAt i =
          let diag = i + cycleLen - 2 * row
           in if row > 0 && row < numRows - 1 && diag < length s
                then [s !! i, s !! diag]
                else [s !! i]

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls
    then pure ()
    else do
      let t = read (head ls) :: Int
      putStr (go t (tail ls))
  where
    go 0 _ = ""
    go n (s:r:rest) =
      let line = convertZigzag s (read r)
       in if n == 1 then line else line ++ "\n" ++ go (n - 1) rest
    go _ _ = ""
