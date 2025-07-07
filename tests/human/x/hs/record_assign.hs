module Main where

import Data.IORef

data Counter = Counter { n :: IORef Int }

inc :: Counter -> IO ()
inc c = modifyIORef' (n c) (+1)

main :: IO ()
main = do
  ref <- newIORef 0
  let c = Counter ref
  inc c
  readIORef ref >>= print
