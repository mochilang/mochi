{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Data.List (intercalate, isInfixOf, union, intersect, nub, sortOn, (\\))
a = [1, 2]
main :: IO ()
main = do print (a ++ [3])
