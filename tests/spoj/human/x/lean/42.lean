/- Solution for SPOJ ADDREV - Adding Reversed Numbers
https://www.spoj.com/problems/ADDREV/
-/

import Std
open Std

partial def revNat (n : Nat) : Nat :=
  let rec loop (n acc : Nat) :=
    if n == 0 then acc
    else loop (n / 10) (acc * 10 + n % 10)
  loop n 0

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    let parts := line.splitOn " "
    let a := (parts[0]!).toNat!
    let b := (parts[1]!).toNat!
    let sum := revNat a + revNat b
    IO.println <| revNat sum
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
