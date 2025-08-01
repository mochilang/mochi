{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
main :: IO ()
main = do mapM_ (\ i -> do print (i)) [1 .. (4 - 1)]
