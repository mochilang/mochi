module Main where

import qualified Data.Map as M

main :: IO ()
main = do
  let m = M.fromList [("a", 1), ("b", 2)]
  mapM_ putStrLn (M.keys m)
