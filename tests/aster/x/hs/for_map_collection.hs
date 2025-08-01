{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import qualified Data.Map as Map
m = Map.fromList [("a", 1), ("b", 2)]
main :: IO ()
main = do mapM_ (\ k -> do putStrLn k) (Map.keys m)
