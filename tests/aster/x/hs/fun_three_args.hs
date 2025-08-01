{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
sum3 a b c = a + b + c
main :: IO ()
main = do print (sum3 1 2 3)
