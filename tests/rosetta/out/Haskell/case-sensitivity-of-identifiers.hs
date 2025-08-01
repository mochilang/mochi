-- Generated by Mochi compiler v0.10.26 on 2025-07-16T09:30:32Z
-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data AnyValue = VInt Int | VDouble Double | VString String | VBool Bool deriving (Show)

_asInt :: AnyValue -> Int
_asInt (VInt n) = n
_asInt v = error ("expected int, got " ++ show v)

_asDouble :: AnyValue -> Double
_asDouble (VDouble d) = d
_asDouble v = error ("expected double, got " ++ show v)

_asString :: AnyValue -> String
_asString (VString s) = s
_asString v = error ("expected string, got " ++ show v)

_asBool :: AnyValue -> Bool
_asBool (VBool b) = b
_asBool v = error ("expected bool, got " ++ show v)

_showAny :: AnyValue -> String
_showAny (VInt n) = show n
_showAny (VDouble d) = show d
_showAny (VString s) = s
_showAny (VBool b) = if b then "true" else "false"

main :: ()
main =
  fromMaybe (()) $
    (let pkg_dog = "Salt" in (let Dog = "Pepper" in (let pkg_DOG = "Mustard" in (let packageSees = (\d1 d2 d3 -> fromMaybe (()) $ case (let _ = putStrLn (_showAny (((((("Package sees: " + d1) + " ") + d2) + " ") + d3))) in Nothing) of Just v -> Just v; Nothing -> Just (Map.fromList [("pkg_dog", True), ("Dog", True), ("pkg_DOG", True)])) in (let d = packageSees pkg_dog Dog pkg_DOG in case (let _ = putStrLn ((("There are " ++ show length d) ++ " dogs.\n")) in Nothing) of Just v -> Just v; Nothing -> (let dog = "Benjamin" in (let d = packageSees pkg_dog Dog pkg_DOG in case (let _ = putStrLn (_showAny (((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))) in Nothing) of Just v -> Just v; Nothing -> (let d = Map.insert "dog" True d in (let d = Map.insert "Dog" True d in (let d = Map.insert "pkg_DOG" True d in case (let _ = putStrLn ((("There are " ++ show length d) ++ " dogs.\n")) in Nothing) of Just v -> Just v; Nothing -> (let Dog = "Samba" in (let d = packageSees pkg_dog Dog pkg_DOG in case (let _ = putStrLn (_showAny (((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))) in Nothing) of Just v -> Just v; Nothing -> (let d = Map.insert "dog" True d in (let d = Map.insert "Dog" True d in (let d = Map.insert "pkg_DOG" True d in case (let _ = putStrLn ((("There are " ++ show length d) ++ " dogs.\n")) in Nothing) of Just v -> Just v; Nothing -> (let DOG = "Bernie" in (let d = packageSees pkg_dog Dog pkg_DOG in case (let _ = putStrLn (_showAny (((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG))) in Nothing) of Just v -> Just v; Nothing -> (let d = Map.insert "dog" True d in (let d = Map.insert "Dog" True d in (let d = Map.insert "pkg_DOG" True d in (let d = Map.insert "DOG" True d in (let _ = putStrLn ((("There are " ++ show length d) ++ " dogs.")) in Nothing))))))))))))))))))))))

main :: IO ()
main = do
  main
