import Data.List (intercalate)
justify :: [String] -> Int -> [String]
justify words maxW = go words where
  go [] = []
  go ws = let (lineWords, rest) = takeLine ws 0 []
              total = sum (map length lineWords)
              gaps = length lineWords - 1
              line = if null rest || gaps == 0
                     then let s = intercalate " " lineWords in s ++ replicate (maxW - length s) ' '
                     else let spaces = maxW - total; base = spaces `div` gaps; extra = spaces `mod` gaps in build lineWords base extra
          in line : go rest
  takeLine [] _ acc = (reverse acc, [])
  takeLine (w:ws) total acc = let cnt = length acc in if total + length w + cnt <= maxW then takeLine ws (total + length w) (w:acc) else (reverse acc, w:ws)
  build [w] _ _ = w
  build (w:ws) base extra = w ++ replicate (base + if extra > 0 then 1 else 0) ' ' ++ build ws base (max 0 (extra - 1))
main :: IO ()
main = interact $ unlines . solve . lines where solve [] = []; solve (tStr:rest) = go (read tStr) rest; go 0 _ = []; go t (nStr:xs) = let n = read nStr; ws = take n xs; width = read (xs !! n); ans = justify ws width; block = show (length ans) : map (\s -> "|" ++ s ++ "|") ans in block ++ (if t > 1 then ["="] else []) ++ go (t-1) (drop (n+1) xs); go _ _ = []
