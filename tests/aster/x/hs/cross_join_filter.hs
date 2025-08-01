{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (l, n)
data GenType1 = GenType1 {n :: Int, l :: String }
letters = ["A", "B"]
nums = [1, 2, 3]
pairs = [GenType1{n = n, l = l} | n <- nums, l <- letters, n `mod` 2 == 0]
main :: IO ()
main = do putStrLn "--- Even pairs ---"
   mapM_ (\ p -> do putStrLn (show p . n ++ " " ++ p . l)) pairs
