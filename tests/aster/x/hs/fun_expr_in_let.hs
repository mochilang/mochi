{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
square = \ x -> x * x
main :: IO ()
main = do print (square 6)
