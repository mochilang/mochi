{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
main :: IO ()
main = do let loop [] = return ()
       loop (n : xs)
         | n `mod` 2 == 0 = loop xs
         | n > 7 = return ()
         | otherwise =
           do putStrLn ("odd number:" ++ " " ++ show n)
              loop xs
   loop numbers
