/- Solution for SPOJ TREEOI14 - Trees
https://www.spoj.com/problems/TREEOI14/
-/

import Std
open Std

/-- compute total disorder coefficient of array -/
def totalCost (a : Array Int) : Nat :=
  Id.run do
    let n := a.size
    let mut s : Nat := 0
    for i in [0:n-1] do
      let x := a[i]!
      let y := a[i+1]!
      s := s + (x - y).natAbs
    return s

/-- swap elements at positions i and j (0-indexed) -/
def swapArr (a : Array Int) (i j : Nat) : Array Int :=
  let x := a[i]!
  let y := a[j]!
  (a.set! i y).set! j x

def main : IO Unit := do
  let h ← IO.getStdin
  let n := (← h.getLine).trim.toNat!
  let nums := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
  let mut arr : Array Int := #[]
  for s in nums do
    arr := arr.push s.toInt!
  let base := totalCost arr
  for i in [0:n] do
    let mut best := base
    for j in [0:n] do
      let c := totalCost (swapArr arr i j)
      if c < best then
        best := c
    IO.println best
