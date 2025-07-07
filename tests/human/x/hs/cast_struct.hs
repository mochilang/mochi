module Main where

data Todo = Todo { title :: String }

main :: IO ()
main = do
  let todo = Todo { title = "hi" }
  putStrLn (title todo)
