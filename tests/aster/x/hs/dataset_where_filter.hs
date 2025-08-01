{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (age, is_senior, name)
data GenType1 = GenType1 {name :: String, age :: Int }
data GenType2 = GenType2 {name :: String, age :: Int, is_senior :: Bool }
adults = [GenType2{name = person . name, age = person . age,
          is_senior = person . age >= 60}
 | person <- people, person . age >= 18]
people = [GenType1{name = "Alice", age = 30},
 GenType1{name = "Bob", age = 15},
 GenType1{name = "Charlie", age = 65},
 GenType1{name = "Diana", age = 45}]
main :: IO ()
main = do putStrLn "--- Adults ---"
   mapM_
     (\ person ->
        do putStrLn
             (person . name ++
                " " ++
                  "is" ++
                    " " ++
                      show person . age ++
                        " " ++ if person . is_senior then " (senior)" else ""))
     adults
