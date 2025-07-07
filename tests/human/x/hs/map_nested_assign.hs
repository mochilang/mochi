module Main where

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let start = M.fromList [("outer", M.fromList [("inner",1)])] :: M.Map String (M.Map String Int)
      updated = M.adjust (M.insert "inner" 2) "outer" start
  print ( (updated M.! "outer") M.! "inner" )
