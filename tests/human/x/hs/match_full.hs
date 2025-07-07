module Main where

classify :: Int -> String
classify n = case n of
  0 -> "zero"
  1 -> "one"
  _ -> "many"

main :: IO ()
main = do
  let x = 2
      label = case x of
                 1 -> "one"
                 2 -> "two"
                 3 -> "three"
                 _ -> "unknown"
  putStrLn label

  let day = "sun"
      mood = case day of
                "mon" -> "tired"
                "fri" -> "excited"
                "sun" -> "relaxed"
                _     -> "normal"
  putStrLn mood

  let ok = True
      status = if ok then "confirmed" else "denied"
  putStrLn status

  putStrLn (classify 0)
