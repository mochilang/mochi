/- Solution for SPOJ PALACE - Accomodate the palace
https://www.spoj.com/problems/PALACE/
-/

import Std
open Std

/-- modular exponentiation by repeated squaring --/
partial def powMod (b e m : Nat) : Nat :=
  if e == 0 then 1 % m
  else
    let h := powMod b (e / 2) m
    let hh := (h * h) % m
    if e % 2 == 0 then hh else (hh * (b % m)) % m

/-- process K test cases reading from the given stream --/
partial def process (h : IO.FS.Stream) (k : Nat) : IO Unit := do
  if k == 0 then
    pure ()
  else
    let nLine ← h.getLine
    let n := nLine.trim.toNat!
    let exp := (n - 1) * (n - 1)
    IO.println (powMod 2 exp 98777)
    process h (k - 1)

/-- main entry point --/
def main : IO Unit := do
  let h ← IO.getStdin
  let kLine ← h.getLine
  process h (kLine.trim.toNat!)
