{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (cat, flag, share, val)
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
data MGroup = MGroup {key :: k, items :: [a] }
data GenType1 = GenType1 {cat :: String, val :: Int, flag :: Bool }
data GenType2 = GenType2 {cat :: String, share :: Double }
items = [GenType1{cat = "a", val = 10, flag = True},
 GenType1{cat = "a", val = 5, flag = False},
 GenType1{cat = "b", val = 20, flag = True}]
result = sortOn (\ g -> g . key)
  [GenType2{cat = g . key,
            share =
              sum [if x . flag then x . val else 0 | x <- g . items] `div`
                sum [x . val | x <- g . items]}
   |
   g <- [MGroup{key = k, items = [i | i <- items, i . cat == k]} |
         k <- nub ([i . cat | i <- items])]]
main :: IO ()
main = do print (result)
