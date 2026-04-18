import Data.List (delete)

getPermutation :: Int -> Int -> String
getPermutation n kInput = go [1..n] (kInput - 1) n facts
  where
    facts = scanl (*) 1 [1..n]
    go _ _ 0 _ = ""
    go digits k rem fs =
      let block = fs !! (rem - 1)
          idx = k `div` block
          k' = k `mod` block
          pick = digits !! idx
      in show pick ++ go (delete pick digits) k' (rem - 1) fs

solveInput :: [String] -> [String]
solveInput [] = []
solveInput (tStr:rest) = go (read tStr) rest
  where
    go 0 _ = []
    go t (nStr:kStr:xs) = getPermutation (read nStr) (read kStr) : go (t - 1) xs
    go _ _ = []

main :: IO ()
main = interact $ unlines . solveInput . lines
