addLists :: [Int] -> [Int] -> Int -> [Int]
addLists [] [] 0 = []
addLists [] [] carry = [carry]
addLists (x:xs) [] carry = let s = x + carry in mod s 10 : addLists xs [] (div s 10)
addLists [] (y:ys) carry = let s = y + carry in mod s 10 : addLists [] ys (div s 10)
addLists (x:xs) (y:ys) carry = let s = x + y + carry in mod s 10 : addLists xs ys (div s 10)

fmt :: [Int] -> String
fmt xs = "[" ++ go xs ++ "]" where
  go [] = ""
  go [x] = show x
  go (x:rest) = show x ++ "," ++ go rest

solve :: Int -> [String] -> [String]
solve 0 _ = []
solve t (n:rest) =
  let n' = read n
      a = map read (take n' rest)
      rest1 = drop n' rest
      m' = read (head rest1)
      b = map read (take m' (tail rest1))
      rest2 = drop m' (tail rest1)
  in fmt (addLists a b 0) : solve (t - 1) rest2

main :: IO ()
main = do
  contents <- getContents
  let vals = words contents
  case vals of
    [] -> pure ()
    (t:rest) -> putStrLn (foldr1 (\a b -> a ++ "\n" ++ b) (solve (read t) rest))
