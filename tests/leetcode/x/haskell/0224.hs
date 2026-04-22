import Data.Char (isDigit, ord)

calculate :: String -> Int
calculate expr = go expr 0 0 1 []
  where
    go [] result number sign _ = result + sign * number
    go (ch:rest) result number sign stack
      | isDigit ch = go rest result (number * 10 + ord ch - ord '0') sign stack
      | ch == '+' = go rest (result + sign * number) 0 1 stack
      | ch == '-' = go rest (result + sign * number) 0 (-1) stack
      | ch == '(' = go rest 0 0 1 (sign : result : stack)
      | ch == ')' =
          let result2 = result + sign * number
              prevSign = head stack
              prevResult = head (tail stack)
          in go rest (prevResult + prevSign * result2) 0 1 (drop 2 stack)
      | otherwise = go rest result number sign stack

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls
    then return ()
    else do
      let t = read (head ls) :: Int
      putStr $ unlines $ map (show . calculate) $ take t (tail ls)
