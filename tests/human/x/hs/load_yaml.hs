{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (FromJSON)

data Person = Person { name :: String, age :: Int, email :: String }
  deriving (Show, Generic)

instance FromJSON Person

main :: IO ()
main = do
  bs <- BS.readFile "../../interpreter/valid/people.yaml"
  case Y.decodeEither' bs of
    Left _ -> return ()
    Right (people :: [Person]) ->
      mapM_ (\p ->
               if age p >= 18 then putStrLn (name p ++ " " ++ email p) else return ())
            people
