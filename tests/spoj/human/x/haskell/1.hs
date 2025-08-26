-- Solution for SPOJ TEST - Life, the Universe, and Everything
-- https://www.spoj.com/problems/TEST

import System.IO (isEOF)

main :: IO ()
main = loop
  where
    loop = do
        eof <- isEOF
        if eof
            then return ()
            else do
                line <- getLine
                if line == "42"
                    then return ()
                    else putStrLn line >> loop
