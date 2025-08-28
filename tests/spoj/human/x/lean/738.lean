/- Solution for SPOJ TREE - Another Counting Problem
https://www.spoj.com/problems/TREE/
-/

import Std
open Std

set_option linter.deprecated false

/-- helper loop computing S(d) = 1 + S(d-1)^n iteratively -/
@[inline]
def sLoop (n : Nat) : Nat → Nat → Nat
| 0, acc => acc
| Nat.succ k, acc => sLoop n k (1 + acc ^ n)

/-- number of strictly n-ary trees of depth at most `d`. -/
def countS (n d : Nat) : Nat := sLoop n d 1

/-- number of strictly n-ary trees of depth exactly `d`. -/
def countExact (n d : Nat) : Nat :=
  match d with
  | 0 => 1
  | Nat.succ k =>
      let prev := countS n k
      let cur := 1 + prev ^ n
      cur - prev

partial def process (h : IO.FS.Stream) : IO Unit := do
  let line := (← h.getLine).trim
  let parts := line.split (fun c => c = ' ')
  let n := (parts[0]!).toNat!
  let d := (parts[1]!).toNat!
  if n = 0 && d = 0 then
    pure ()
  else
    let ans := countExact n d
    IO.println s!"{n} {d} {ans}"
    process h

def main : IO Unit := do
  let h ← IO.getStdin
  process h
