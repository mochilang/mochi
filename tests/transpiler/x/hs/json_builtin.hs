{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (a, b)
-- Generated by Mochi transpiler v0.10.35 on 2025-07-22 16:14 GMT+7
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
data GenType1 = GenType1
  { a :: Int,
    b :: Int
  } deriving (Show, Eq)


m = GenType1 {a = 1, b = 2}

main :: IO ()
main = do
    BSL.putStrLn (Aeson.encode m)
