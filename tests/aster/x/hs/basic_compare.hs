{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
a = 10 - 3
b = 2 + 2
main :: IO ()
main = do print (a)
   print (a == 7)
   print (b < 5)
