-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

data Book = Book
  { title :: String,
    author :: Person
  }
  deriving (Eq, Show, Generic)

book = Book {title = "Go", author = Person {name = "Bob", age = 42}}

main :: IO ()
main = do
  putStrLn (name (author (book)))
