{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:46 GMT+7
inc x = x + k

k = 2

main :: IO ()
main = do
    print (inc 3)
