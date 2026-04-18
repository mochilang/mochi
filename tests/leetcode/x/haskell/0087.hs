import qualified Data.Map as M
import Data.List (sort)

solve :: String -> String -> Bool
solve s1 s2 = fst (dfs 0 0 (length s1) M.empty) where
  dfs i1 i2 len memo =
    case M.lookup (i1, i2, len) memo of
      Just v -> (v, memo)
      Nothing ->
        let a = take len (drop i1 s1)
            b = take len (drop i2 s2)
        in if a == b then (True, M.insert (i1, i2, len) True memo)
           else if sort a /= sort b then (False, M.insert (i1, i2, len) False memo)
           else tryK 1 memo
        where
          tryK k memo
            | k >= len = (False, M.insert (i1, i2, len) False memo)
            | otherwise =
                let (a1, m1) = dfs i1 i2 k memo
                    (a2, m2) = dfs (i1 + k) (i2 + k) (len - k) m1
                in if a1 && a2 then (True, M.insert (i1, i2, len) True m2)
                   else let (b1, m3) = dfs i1 (i2 + len - k) k m2
                            (b2, m4) = dfs (i1 + k) i2 (len - k) m3
                        in if b1 && b2 then (True, M.insert (i1, i2, len) True m4) else tryK (k + 1) m4

joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs

main :: IO ()
main = do
  ls <- lines <$> getContents
  case ls of
    [] -> pure ()
    tStr:rest -> do
      let t = read tStr :: Int
      putStr $ joinLines [if solve (rest !! (2 * i)) (rest !! (2 * i + 1)) then "true" else "false" | i <- [0 .. t - 1]]
