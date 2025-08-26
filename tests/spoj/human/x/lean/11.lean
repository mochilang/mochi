/- Solution for SPOJ FCTRL - Factorial
https://www.spoj.com/problems/FCTRL/
-/

import Std
open Std

partial def zeros (n : Nat) : Nat :=
  let rec go (k acc : Nat) : Nat :=
    if k == 0 then acc
    else
      let k' := k / 5
      go k' (acc + k')
  go n 0

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (zeros n)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
