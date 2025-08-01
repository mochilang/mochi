{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Debug.Trace (trace)
boom = trace "boom" True
main :: IO ()
main = do print (fromEnum ((1 < 2) && (2 < 3) && (3 < 4)))
   print (fromEnum ((1 < 2) && (2 > 3) && boom))
   print (fromEnum ((1 < 2) && (2 < 3) && (3 > 4) && boom))
