/- Solution for SPOJ EIGHTS - Triple Fat Ladies
https://www.spoj.com/problems/EIGHTS/
-/

import Std
open Std

/-- Compute kth number whose cube ends with 888.
    The sequence starts at 192 and increases by 250. -/
def kth (k : Nat) : Nat :=
  192 + (k - 1) * 250

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let k := (← h.getLine).trim.toNat!
    IO.println (kth k)
