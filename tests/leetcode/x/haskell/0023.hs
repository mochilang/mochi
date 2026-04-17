import Data.List
solve :: [String] -> [String]
solve [] = []
solve (tStr:rest) = go (read tStr :: Int) rest where
  go 0 _ = []
  go n (kStr:xs) = let (vals, rems) = readLists (read kStr :: Int) xs in ("[" ++ intercalate "," (map show (sort vals)) ++ "]") : go (n-1) rems
  go _ _ = []
  readLists :: Int -> [String] -> ([Int], [String])
  readLists 0 ys = ([], ys)
  readLists k (nStr:ys) = let n = read nStr :: Int; (a,b) = splitAt n ys; (restVals, rems) = readLists (k-1) b in (map read a ++ restVals, rems)
  readLists _ _ = ([], [])
main = interact (intercalate "\n" . solve . lines)
