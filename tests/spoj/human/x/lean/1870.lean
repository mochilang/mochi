/- Solution for SPOJ MKLABELS - Making Labels
https://www.spoj.com/problems/MKLABELS/
-/

import Std
open Std

partial def loop (case : Nat) (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let count := n ^ (n - 2)
    IO.println s!"Case {case}, N = {n}, # of different labelings = {count}"
    loop (case + 1) h

def main : IO Unit := do
  loop 1 (← IO.getStdin)
