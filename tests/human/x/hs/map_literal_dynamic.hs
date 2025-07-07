module Main where

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let x = 3
      y = 4
      m = M.fromList [("a", x), ("b", y)] :: M.Map String Int
  print (m M.! "a", m M.! "b")
