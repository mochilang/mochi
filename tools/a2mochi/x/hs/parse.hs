{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

patternName :: H.Name l -> String
patternName (H.Ident _ s)  = s
patternName (H.Symbol _ s) = s

patToName :: H.Pat l -> String
patToName (H.PVar _ n) = patternName n
patToName _          = ""

rhsToString :: H.Rhs l -> String
rhsToString (H.UnGuardedRhs _ e) = H.prettyPrint e
rhsToString _ = ""

declToItem :: H.Decl l -> Maybe Item
-- type signatures
declToItem (H.TypeSig _ [n] t) =
  Just Item{ kind="sig", name=patternName n, params=[], body="", typ=H.prettyPrint t, fields=[] }
-- simple function bindings
declToItem (H.FunBind _ (H.Match _ n ps rhs _ : _)) =
  Just Item{ kind="func", name=patternName n, params=map patToName ps, body=rhsToString rhs, typ="", fields=[] }
-- pattern bindings
declToItem (H.PatBind _ (H.PVar _ n) rhs _) =
  Just Item{ kind="var", name=patternName n, params=[], body=rhsToString rhs, typ="", fields=[] }
-- data declarations (only single record constructor supported)
declToItem (H.DataDecl _ _ _ dh cons _) = case cons of
  [H.QualConDecl _ _ _ (H.RecDecl _ _ fs)] ->
    Just Item{ kind="struct", name=declHeadName dh, params=[], body="", typ="", fields=concatMap convField fs }
  _ -> Nothing
  where
    convField (H.FieldDecl _ ns ty) = [Field (patternName f) (H.prettyPrint ty) | f <- ns]
    declHeadName (H.DHead _ n) = patternName n
    declHeadName (H.DHInfix _ _ n) = patternName n
    declHeadName (H.DHParen _ h) = declHeadName h
    declHeadName (H.DHApp _ h _) = declHeadName h
-- ignore other declarations
declToItem _ = Nothing
