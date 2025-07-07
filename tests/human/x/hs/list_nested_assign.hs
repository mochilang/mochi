replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs = take i xs ++ [val] ++ drop (i+1) xs

main :: IO ()
main = do
  let matrix = [[1,2],[3,4]] :: [[Int]]
      row1 = matrix !! 1
      row1' = replaceAt 0 5 row1
      matrix2 = replaceAt 1 row1' matrix
  print ((matrix2 !! 1) !! 0)
