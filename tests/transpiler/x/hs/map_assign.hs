{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Generated by Mochi transpiler v0.10.34 on 2025-07-22 06:45 GMT+7
import qualified Data.Map as Map
scores = Map.insert "bob" 2 (Map.fromList [("alice", 1)])

main :: IO ()
main = do
    print ((scores Map.! "bob"))
