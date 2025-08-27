/- Solution for SPOJ INTERVAL - Intervals
https://www.spoj.com/problems/INTERVAL/
-/
import Std
open Std

structure Interval where
  a : Nat
  b : Nat
  c : Nat

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (root, parent') := find parent px
    (root, parent'.set! x root)

def lowbit (x : Nat) : Nat :=
  x - Nat.land x (x - 1)

def bitUpdate (bit : Array Nat) (n idx delta : Nat) : Array Nat :=
  Id.run do
    let mut b := bit
    let mut i := idx
    while _h : i <= n do
      b := b.set! i (b[i]! + delta)
      let lb := lowbit i
      i := i + lb
    pure b

def bitQuery (bit : Array Nat) (idx : Nat) : Nat :=
  Id.run do
    let mut s := 0
    let mut i := idx
    while _h : i > 0 do
      s := s + bit[i]!
      let lb := lowbit i
      i := i - lb
    pure s

partial def solveCase (h : IO.FS.Stream) (n : Nat) : IO Nat := do
  let mut arr : Array Interval := Array.mkEmpty n
  for _ in [0:n] do
    let ln := (← h.getLine).trim
    let ps := (ln.splitOn " ").toArray
    let a := ps[0]! |>.toNat!
    let b := ps[1]! |>.toNat!
    let c := ps[2]! |>.toNat!
    arr := arr.push {a := a, b := b, c := c}
  let arrSorted := arr.qsort (fun x y => x.b < y.b)
  let maxIdx := 50001
  let size := maxIdx + 1
  let mut bit : Array Nat := Array.replicate (size + 1) 0
  let mut parent : Array Nat := Array.replicate (size + 1) 0
  for i in [0:size+1] do
    parent := parent.set! i i
  let mut total := 0
  for iv in arrSorted do
    let a1 := iv.a + 1
    let b1 := iv.b + 1
    let existing := bitQuery bit b1 - bitQuery bit (a1 - 1)
    let mut need := if iv.c > existing then iv.c - existing else 0
    total := total + need
    let mut x := b1
    while _h : need > 0 do
      let (p, parent1) := find parent x
      parent := parent1
      bit := bitUpdate bit maxIdx p 1
      let (rootPrev, parent2) := find parent (p - 1)
      parent := parent2.set! p rootPrev
      need := need - 1
      x := p - 1
  pure total

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let n := line.toNat!
      let ans ← solveCase h n
      IO.println ans
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
