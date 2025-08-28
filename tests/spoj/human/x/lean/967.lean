/- Solution for SPOJ PB - Parking Bay
https://www.spoj.com/problems/PB/
-/

import Std
open Std

/-- fast modular exponentiation --/
partial def powMod (a b m : Nat) : Nat :=
  if b == 0 then 1 % m
  else
    let h := powMod a (b / 2) m
    let hh := (h * h) % m
    if b % 2 == 0 then hh else (hh * (a % m)) % m

def MOD : Nat := 10007

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    let res := powMod (n + 1) (n - 1) MOD
    IO.println res
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let line ← h.getLine
  process h (line.trim.toNat!)
