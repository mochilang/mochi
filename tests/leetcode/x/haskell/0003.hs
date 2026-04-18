import qualified Data.Map.Strict as M

longest :: String -> Int
longest s = go s 0 0 M.empty 0 where
  go [] _ _ _ best = best
  go (c:cs) right left lastSeen best =
    let left2 = case M.lookup c lastSeen of
                  Just pos | pos >= left -> pos + 1
                  _ -> left
        best2 = max best (right - left2 + 1)
    in go cs (right + 1) left2 (M.insert c right lastSeen) best2

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  case ls of
    [] -> pure ()
    (t:rest) -> putStrLn (unlines' (map (show . longest) (take (read t) rest)))
  where unlines' = foldr1 (\a b -> a ++ "\n" ++ b)
