{-# LANGUAGE DeriveGeneric #-}
import System.Environment (getArgs)
import Data.Aeson (encode, ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Language.Haskell.Exts as H
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)

-- Field represents a record field in a data declaration
data Field = Field { fname :: String, ftype :: String } deriving (Generic, Show)

instance ToJSON Field where
  toJSON (Field n t) = object ["name" .= n, "type" .= t]

-- Item is a simplified AST node used by the Go converter
data Item = Item
  { kind  :: String
  , name  :: String
  , params :: [String]
  , body  :: String
  , typ   :: String
  , fields :: [Field]
  } deriving (Generic, Show)

instance ToJSON Item where
  toJSON it = object
    [ "kind"   .= kind it
    , "name"   .= name it
    , "params" .= params it
    , "body"   .= body it
    , "type"   .= typ it
    , "fields" .= fields it
    ]

main :: IO ()
main = do
  [file] <- getArgs
  src <- readFile file
  let mode = H.defaultParseMode { H.parseFilename = file }
  case H.parseModuleWithMode mode src of
    H.ParseFailed loc msg -> error (show loc ++ msg)
    H.ParseOk (H.Module _ _ _ _ decls) ->
      BL.putStrLn (encode (mapMaybe declToItem decls))

patternName :: H.Name -> String
patternName (H.Ident s)  = s
patternName (H.Symbol s) = s

patToName :: H.Pat -> String
patToName (H.PVar n) = patternName n
patToName _          = ""

rhsToString :: H.Rhs -> String
rhsToString (H.UnGuardedRhs e) = H.prettyPrint e
rhsToString _ = ""

declToItem :: H.Decl -> Maybe Item
-- type signatures
declToItem (H.TypeSig _ [n] t) =
  Just Item{ kind="sig", name=patternName n, params=[], body="", typ=H.prettyPrint t, fields=[] }
-- simple function bindings
declToItem (H.FunBind (H.Match n ps rhs _ : _)) =
  Just Item{ kind="func", name=patternName n, params=map patToName ps, body=rhsToString rhs, typ="", fields=[] }
-- pattern bindings
declToItem (H.PatBind (H.PVar n) rhs _) =
  Just Item{ kind="var", name=patternName n, params=[], body=rhsToString rhs, typ="", fields=[] }
-- data declarations (only single record constructor supported)
declToItem (H.DataDecl _ _ _ name _ [H.QualConDecl _ _ _ (H.RecDecl _ fs)] _) =
  Just Item{ kind="struct", name=patternName name, params=[], body="", typ="", fields=concatMap convField fs }
  where convField (ns, bty) = [Field (patternName f) (bangToStr bty) | f <- ns]
        bangToStr (H.BangedTy t)   = H.prettyPrint t
        bangToStr (H.UnBangedTy t) = H.prettyPrint t
        bangToStr (H.UnpackedTy t) = H.prettyPrint t
-- ignore other declarations
declToItem _ = Nothing
