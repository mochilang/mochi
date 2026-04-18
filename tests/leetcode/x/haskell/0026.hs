import Data.List (intercalate, group)

removeDuplicates :: [Int] -> [Int]
removeDuplicates = map head . group

main :: IO ()
main = do
    input <- getContents
    let tokens = words input
    if null tokens then return ()
    else do
        let (t:rem) = map read tokens
        solve t rem

solve :: Int -> [Int] -> IO ()
solve 0 _ = return ()
solve t (n:rem) = do
    let (nums, rem') = splitAt n rem
    let ans = removeDuplicates nums
    putStrLn $ intercalate " " (map show ans)
    solve (t - 1) rem'
