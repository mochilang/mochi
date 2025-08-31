/- Solution for SPOJ TCOUNT3 - Counting Triangles III
https://www.spoj.com/problems/TCOUNT3/
-/

import Std
open Std

-- compute number of triangles in level N hexagram
-- formula: (44 n^3 + 33 n^2 + 4 n - (n mod 2)) / 4

def countHexagram (n : Nat) : Nat :=
  let n2 := n * n
  let n3 := n2 * n
  let base := 44 * n3 + 33 * n2 + 4 * n
  let num := base - (n % 2)
  num / 4

partial def process (h : IO.FS.Stream) : Nat -> IO Unit
| 0 => pure ()
| Nat.succ t => do
    let line := (← h.getLine).trim
    let n := line.toNat!
    IO.println (toString (countHexagram n))
    process h t

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
