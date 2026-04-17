import Data.List (intercalate)

twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = head [(i, j) | i <- [0 .. length nums - 1], j <- [i + 1 .. length nums - 1], nums !! i + nums !! j == target]

solve :: [Int] -> Int -> [String]
solve _ 0 = []
solve (n:target:rest) t =
  let nums = take n rest
      tailVals = drop n rest
      (a, b) = twoSum nums target
  in (show a ++ " " ++ show b) : solve tailVals (t - 1)
solve _ _ = []

main :: IO ()
main = do
  contents <- getContents
  let vals = map read (words contents) :: [Int]
  case vals of
    [] -> pure ()
    (t:rest) -> putStr (intercalate "\n" (solve rest t))
