{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (n)
-- Generated by Mochi transpiler v0.10.36 on 2025-07-22 17:41 GMT+7
data Counter = Counter
  { n :: Int
  } deriving (Show, Eq)


inc c = do
    c = c {n = c.n + 1}


c = Counter {n = 0}

main :: IO ()
main = do
    inc c
    print (c.n)
