{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Generated by Mochi transpiler v0.10.34 on 2025-07-22 08:57 GMT+7
testpkg_Add a b = a + b
testpkg_Answer = 42

main :: IO ()
main = do
    print (testpkg_Add 2 3)
    print (testpkg_Pi)
    print (testpkg_Answer)
