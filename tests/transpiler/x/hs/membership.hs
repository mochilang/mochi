{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Generated by Mochi transpiler v0.10.34 on 2025-07-22 09:24 GMT+7
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
nums = [1, 2, 3]

main :: IO ()
main = do
    print (fromEnum (2 `elem` nums))
    print (fromEnum (4 `elem` nums))
