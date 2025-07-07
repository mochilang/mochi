module Main where

import qualified Data.Map as M

main :: IO ()
main = print (M.elems (M.fromList [("a",1),("b",2),("c",3)]))
