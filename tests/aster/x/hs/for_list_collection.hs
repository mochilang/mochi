{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
main :: IO ()
main = do mapM_ (\ n -> do print (n)) [1, 2, 3]
