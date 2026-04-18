import Data.List
revGroups :: [Int] -> Int -> [Int]
revGroups [] _ = []
revGroups xs k | length xs < k = xs
revGroups xs k = reverse a ++ revGroups b k where (a,b) = splitAt k xs
fmt xs = "[" ++ intercalate "," (map show xs) ++ "]"
solve [] = []
solve (tStr:rest) = go (read tStr :: Int) rest where
  go 0 _ = []
  go n (nStr:xs) = let m = read nStr :: Int; (a,b) = splitAt m xs; k = read (head b) :: Int in fmt (revGroups (map read a) k) : go (n-1) (tail b)
  go _ _ = []
main = interact (intercalate "\n" . solve . lines)
