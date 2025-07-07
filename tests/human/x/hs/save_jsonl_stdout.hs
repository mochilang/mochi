{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B

people :: [ (String, Int) ]
people = [("Alice",30),("Bob",25)]

main :: IO ()
main = mapM_ (B.putStrLn . encode . \(n,a) -> object ["name" .= n, "age" .= a]) people
