{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
makeAdder n = \ x -> x + n
add10 = makeAdder 10
main :: IO ()
main = do print (add10 7)
