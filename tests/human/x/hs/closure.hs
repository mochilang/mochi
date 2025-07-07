module Main where

makeAdder :: Int -> Int -> Int
makeAdder n = (\x -> x + n)

main :: IO ()
main = do
  let add10 = makeAdder 10
  print (add10 7)
