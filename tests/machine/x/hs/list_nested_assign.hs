-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf, isInfixOf)
import qualified Data.List as List


matrix = [[1, 2], [3, 4]]

main :: IO ()
main = do
    let matrix = _updateAt 1 (\_it0 -> _updateAt 0 (const 5) _it0) matrix
    print (((matrix !! 1) !! 0))

