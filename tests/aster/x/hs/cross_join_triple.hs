{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (b, l, n)
data GenType1 = GenType1 {n :: Int, l :: String, b :: Bool }
bools = [True, False]
combos = [GenType1{n = n, l = l, b = b} | n <- nums, l <- letters,
 b <- bools]
letters = ["A", "B"]
nums = [1, 2]
main :: IO ()
main = do putStrLn "--- Cross Join of three lists ---"
   mapM_
     (\ c ->
        do putStrLn (show c . n ++ " " ++ c . l ++ " " ++ show c . b))
     combos
