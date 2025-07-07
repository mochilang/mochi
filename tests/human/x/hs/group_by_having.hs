module Main where

import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL

data Person = Person { pname :: String, city :: String }

people :: [Person]
people = [ Person "Alice" "Paris"
         , Person "Bob" "Hanoi"
         , Person "Charlie" "Paris"
         , Person "Diana" "Hanoi"
         , Person "Eve" "Paris"
         , Person "Frank" "Hanoi"
         , Person "George" "Paris" ]

groupCities :: M.Map String Int
groupCities = foldl' (\m p -> M.insertWith (+) (city p) 1 m) M.empty people

main :: IO ()
main = do
  let big = [ object ["city" .= c, "num" .= n] | (c,n) <- M.toList groupCities, n >= 4 ]
  BL.putStrLn (encode big)
