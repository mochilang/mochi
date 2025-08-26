/- Solution for SPOJ TEST - Life, the Universe, and Everything
https://www.spoj.com/problems/TEST/
-/

import Std
open Std

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 42 then
    pure ()
  else
    IO.println n
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
