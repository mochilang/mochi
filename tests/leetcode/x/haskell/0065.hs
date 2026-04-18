import Data.Char (isDigit)
isNumber :: String -> Bool
isNumber s = go 0 False False False True where
  n = length s
  go i seenDigit seenDot seenExp digitAfterExp
    | i == n = seenDigit && digitAfterExp
    | isDigit ch = go (i + 1) True seenDot seenExp (if seenExp then True else digitAfterExp)
    | ch == '+' || ch == '-' = if i /= 0 && prev /= 'e' && prev /= 'E' then False else go (i + 1) seenDigit seenDot seenExp digitAfterExp
    | ch == '.' = if seenDot || seenExp then False else go (i + 1) seenDigit True seenExp digitAfterExp
    | ch == 'e' || ch == 'E' = if seenExp || not seenDigit then False else go (i + 1) seenDigit seenDot True False
    | otherwise = False
    where ch = s !! i; prev = if i > 0 then s !! (i - 1) else ' '
main :: IO ()
main = interact $ unlines . solve . lines where solve [] = []; solve (tStr:rest) = [if isNumber (rest !! i) then "true" else "false" | i <- [0 .. read tStr - 1]]
