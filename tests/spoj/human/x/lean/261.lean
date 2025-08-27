/- Solution for SPOJ TRIPART - Triangle Partitioning
https://www.spoj.com/problems/TRIPART/
-/

import Std
open Std

@[inline] def normalize (a b c : Nat) : Nat × Nat × Nat :=
  let arr := #[a, b, c]
  let arr := arr.qsort (fun x y => x > y)
  let x := arr.get! 0
  let y := arr.get! 1
  let z := arr.get! 2
  let g := Nat.gcd x (Nat.gcd y z)
  (x / g, y / g, z / g)

partial def countStyles (a b c : Nat) : Nat :=
  let init := normalize (a*a) (b*b) (c*c)
  let rec bfs (queue : List (Nat × Nat × Nat)) (seen : Std.HashSet (Nat × Nat × Nat)) : Nat :=
    match queue with
    | [] => seen.size
    | (A,B,C) :: qs =>
        let m := 2*B + 2*C - A
        let t1 := normalize A (4*B) m
        let t2 := normalize A (4*C) m
        let (qs, seen) :=
          if seen.contains t1 then (qs, seen) else (t1 :: qs, seen.insert t1)
        let (qs, seen) :=
          if seen.contains t2 then (qs, seen) else (t2 :: qs, seen.insert t2)
        bfs qs seen
  bfs [init] (Std.HashSet.empty.insert init)

partial def process (h : IO.FS.Stream) (cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h cases
    else
      let parts := line.split (· = ' ') |>.filter (· ≠ "")
      let a := parts.get! 0 |>.toNat!
      let b := parts.get! 1 |>.toNat!
      let c := parts.get! 2 |>.toNat!
      IO.println (countStyles a b c)
      process h (cases - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
