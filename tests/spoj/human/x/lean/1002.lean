/- Solution for SPOJ UJ - Uncle Jack
https://www.spoj.com/problems/UJ/
-/

import Std
open Std

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let ws := line.trim.split (· = ' ')
  let n := ws[0]! |>.toNat!
  let d := ws[1]! |>.toNat!
  if n == 0 && d == 0 then
    pure ()
  else
    IO.println (Nat.pow n d)
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
