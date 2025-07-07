replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs = take i xs ++ [val] ++ drop (i+1) xs

main :: IO ()
main = do
  let nums = [1,2] :: [Int]
      nums2 = replaceAt 1 3 nums
  print (nums2 !! 1)
