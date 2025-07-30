{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (encode, Value(..), object, (.=))
import qualified Language.Haskell.Exts as H
import Data.Data
import Data.Text (pack)

conv :: Data a => a -> Value
conv x
  | Just s <- cast x :: Maybe String = String (pack s)
  | Just i <- cast x :: Maybe Int    = Number (fromIntegral i)
  | Just i <- cast x :: Maybe Integer= Number (fromInteger i)
  | Just b <- cast x :: Maybe Bool   = Bool b
  | Just d <- cast x :: Maybe Double = Number (realToFrac d)
  | otherwise =
      let con = showConstr (toConstr x)
          children = gmapQ conv x
      in object (["node" .= con] ++ if null children then [] else ["children" .= children])

main :: IO ()
main = do
  [file] <- getArgs
  src <- readFile file
  let mode = H.defaultParseMode { H.parseFilename = "" }
  case H.parseModuleWithMode mode src of
    H.ParseFailed loc msg -> error (show loc ++ msg)
    H.ParseOk modul -> BL.putStrLn (encode (conv (fmap (const ()) modul)))
