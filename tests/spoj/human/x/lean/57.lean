/- Solution for SPOJ SUPPER - Supernumbers in a permutation
https://www.spoj.com/problems/SUPPER/
-/

import Std
open Std

partial def lowerBound (a : Array Int) (x : Int) : Nat :=
  let rec go (l r : Nat) : Nat :=
    if h : l < r then
      let m := (l + r) / 2
      if a.get! m < x then go (m + 1) r else go l m
    else
      l
  go 0 a.size

def lisForward (a : Array Int) : (Array Nat × Nat) :=
  let n := a.size
  let mut tails : Array Int := #[]
  let mut L : Array Nat := Array.mkArray n 0
  let mut best := 0
  for i in [0:n] do
    let x := a.get! i
    let pos := lowerBound tails x
    if pos == tails.size then
      tails := tails.push x
    else
      tails := tails.set! pos x
    L := L.set! i (pos + 1)
    if pos + 1 > best then
      best := pos + 1
  (L, best)

def lisBackward (a : Array Int) : Array Nat :=
  let n := a.size
  let mut tails : Array Int := #[]
  let mut R : Array Nat := Array.mkArray n 0
  for idx in [0:n] do
    let i := n - 1 - idx
    let x := -a.get! i
    let pos := lowerBound tails x
    if pos == tails.size then
      tails := tails.push x
    else
      tails := tails.set! pos x
    R := R.set! i (pos + 1)
  R

def superNumbers (a : Array Int) : List Int :=
  let (L, lis) := lisForward a
  let R := lisBackward a
  let n := a.size
  let mut res : List Int := []
  for i in [0:n] do
    if L.get! i + R.get! i - 1 == lis then
      res := a.get! i :: res
  res.qsort (fun x y => x < y)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> List.map (fun s => s.toInt!)
               |> List.toArray
  let rec loop (idx : Nat) : IO Unit := do
    if idx ≥ toks.size then
      pure ()
    else
      let n := Int.toNat (toks.get! idx)
      let mut arr : Array Int := Array.mkArray n 0
      for j in [0:n] do
        arr := arr.set! j (toks.get! (idx + 1 + j))
      let nums := superNumbers arr
      IO.println nums.length
      IO.println (String.intercalate " " (nums.map (fun x => toString x)))
      loop (idx + 1 + n)
  loop 0
