{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
_data = [1, 2]
flag = not ((null [x | x <- _data, x == 1]))
main :: IO ()
main = do print (flag)
