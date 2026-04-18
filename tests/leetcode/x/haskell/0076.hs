import qualified Data.IntMap.Strict as M
import Data.Char (ord)
minWindow :: String -> String -> String
minWindow s t = go 0 0 need missing (length s + 1, 0) where
  need = foldl (\m c -> M.insertWith (+) (ord c) 1 m) M.empty t
  missing = length t
  n = length s
  go l r mp miss best
    | r == n = if fst best > n then "" else take (fst best) (drop (snd best) s)
    | otherwise = let c = ord (s !! r); old = M.findWithDefault 0 c mp; mp1 = M.insert c (old - 1) mp; miss1 = if old > 0 then miss - 1 else miss in shrink l (r + 1) mp1 miss1 best
  shrink l r mp miss best
    | miss /= 0 = go l r mp miss best
    | otherwise = let best1 = if r - l < fst best then (r - l, l) else best; c = ord (s !! l); old = M.findWithDefault 0 c mp; mp1 = M.insert c (old + 1) mp; miss1 = if old + 1 > 0 then miss + 1 else miss in if miss1 == 0 then shrink (l + 1) r mp1 miss1 best1 else go (l + 1) r mp1 miss1 best1
main :: IO ()
main = interact $ unlines . solve . lines where solve [] = []; solve (tStr:rest) = [minWindow (rest !! (2*i)) (rest !! (2*i+1)) | i <- [0 .. read tStr - 1]]
