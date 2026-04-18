solve :: [Int] -> Int
solve a = maximum [minimum (take (j - i + 1) (drop i a)) * (j - i + 1) | i <- [0 .. n - 1], j <- [i .. n - 1]]
  where n = length a

run :: Int -> [Int] -> [String]
run 0 _ = []
run t (n:xs) = show (solve (take n xs)) : run (t - 1) (drop n xs)
run _ _ = []

main :: IO ()
main = do
  s <- getContents
  let xs = map read (words s) :: [Int]
  case xs of
    [] -> pure ()
    t:rest -> putStr (unlinesTrim (run t rest))

unlinesTrim :: [String] -> String
unlinesTrim [] = ""
unlinesTrim [x] = x
unlinesTrim (x:xs) = x ++ "\n" ++ unlinesTrim xs
