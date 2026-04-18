valid :: [String] -> Int -> Int -> Char -> Bool
valid b r c ch =
  all (\i -> (b !! r) !! i /= ch && (b !! i) !! c /= ch) [0 .. 8]
    && all (\i -> all (\j -> (b !! i) !! j /= ch) [bc .. bc + 2]) [br .. br + 2]
  where
    br = (r `div` 3) * 3
    bc = (c `div` 3) * 3

setCell :: [String] -> Int -> Int -> Char -> [String]
setCell b r c ch =
  take r b ++ [take c row ++ [ch] ++ drop (c + 1) row] ++ drop (r + 1) b
  where
    row = b !! r

solve :: [String] -> Maybe [String]
solve b =
  case [(r, c) | r <- [0 .. 8], c <- [0 .. 8], (b !! r) !! c == '.'] of
    [] -> Just b
    (r, c) : _ ->
      tryDigits ['1' .. '9']
      where
        tryDigits [] = Nothing
        tryDigits (ch : rest)
          | not (valid b r c ch) = tryDigits rest
          | otherwise =
              case solve (setCell b r c ch) of
                Just ans -> Just ans
                Nothing -> tryDigits rest

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr : rest) = go (read tStr :: Int) rest
  where
    go 0 _ = []
    go n xs =
      let (b, rems) = splitAt 9 xs
       in case solve b of
            Just solved -> solved ++ go (n - 1) rems
            Nothing -> go (n - 1) rems

main :: IO ()
main = interact $ unlines . solveInput . lines
