merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) | a <= b = a : merge as (b:bs)
                       | otherwise = b : merge (a:as) bs

median :: [Int] -> [Int] -> Double
median a b = let m = merge a b; n = length m in if odd n then fromIntegral (m !! (n `div` 2)) else fromIntegral (m !! (n `div` 2 - 1) + m !! (n `div` 2)) / 2.0

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  if null ls then pure () else putStr (solve (read (head ls)) (tail ls))

solve :: Int -> [String] -> String
solve 0 _ = ""
solve t (nStr:rest) =
  let n = read nStr :: Int
      a = map read (take n rest) :: [Int]
      rest1 = drop n rest
      m = read (head rest1) :: Int
      b = map read (take m (tail rest1)) :: [Int]
      rems = drop m (tail rest1)
      line = showFFloat1 (median a b)
  in if t == 1 then line else line ++ "\n" ++ solve (t - 1) rems
solve _ _ = ""

showFFloat1 :: Double -> String
showFFloat1 x = let y = fromIntegral (round (x * 10)) / 10.0 :: Double in if y == fromIntegral (round y :: Int) then show (round y :: Int) ++ ".0" else show y
