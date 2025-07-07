import qualified Data.Map as Map

main :: IO ()
main = do
  let scores = Map.fromList [("alice", 1 :: Int)]
      scores2 = Map.insert "bob" 2 scores
  print (Map.findWithDefault 0 "bob" scores2)
