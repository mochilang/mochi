module Main where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = BL.putStrLn $ encode (object ["a" .= (1 :: Int), "b" .= (2 :: Int)])
