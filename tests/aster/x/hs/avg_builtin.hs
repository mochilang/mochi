{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
main :: IO ()
main = do print
     (fromIntegral (sum [1, 2, 3]) / fromIntegral (length [1, 2, 3]))
