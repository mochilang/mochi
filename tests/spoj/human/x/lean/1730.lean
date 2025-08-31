/- Solution for SPOJ TCOUNT2 - Counting Triangles II
https://www.spoj.com/problems/TCOUNT2/
-/

import Std
open Std

/-- number of triangles in a level `n` hexagon --/
def countTriangles (n : Nat) : Nat :=
  (14 * n * n * n + 9 * n * n + 2 * n - (n % 2)) / 4

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    IO.println (countTriangles n)
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
