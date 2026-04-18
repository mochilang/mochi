import Data.List
solveCase s words' | null words' = [] | otherwise = [i | i <- [0..length s - total], sort [take wlen (drop (i + j*wlen) s) | j <- [0..length words' - 1]] == target]
  where wlen = length (head words'); total = wlen * length words'; target = sort words'
fmt xs = "[" ++ intercalate "," (map show xs) ++ "]"
solve [] = []
solve (tStr:rest) = go (read tStr :: Int) rest where
  go 0 _ = []
  go n (s:mStr:xs) = let m = read mStr :: Int; (w, rems) = splitAt m xs in fmt (solveCase s w) : go (n-1) rems
  go _ _ = []
main = interact (intercalate "\n" . solve . lines)
