import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  let inner = Map.fromList [("inner", 1 :: Int)]
      d0 = Map.fromList [("outer", inner)]
      d1 = Map.adjust (Map.insert "inner" 2) "outer" d0
      result = fromMaybe 0 $ do
        o <- Map.lookup "outer" d1
        Map.lookup "inner" o
  print result
