{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (title)
-- Generated by Mochi transpiler v0.10.34 on 2025-07-22 08:57 GMT+7
import qualified Data.Map as Map
data Todo = Todo
  { title :: String
  } deriving (Show, Eq)


todo = Todo {title = "hi"}

main :: IO ()
main = do
    putStrLn (todo.title)
