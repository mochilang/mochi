{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
import Prelude hiding (title)
import qualified Data.Map as Map
data Todo = Todo {title :: String }
todo = Todo{title = "hi"}
main :: IO ()
main = do putStrLn (todo . title)
