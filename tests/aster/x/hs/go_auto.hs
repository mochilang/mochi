{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
testpkg_Add a b = a + b
testpkg_Answer = 42
testpkg_Pi = 3.14
main :: IO ()
main = do print (testpkg_Add 2 3)
   print (testpkg_Pi)
   print (testpkg_Answer)
