/- Solution for SPOJ BLUEEQ2 - Help Blue Mary Please! (Act II)
https://www.spoj.com/problems/BLUEEQ2/
-/

import Std
open Std

/-- Precompute values `k * x^p` for `1 ≤ x ≤ m`. --/
def precompute (k : Int) (p : Nat) (m : Nat) : Array Int :=
  Id.run do
    let mut arr : Array Int := Array.mkEmpty m
    for x in [1:m+1] do
      let pow := Nat.pow x p
      arr := arr.push (k * Int.ofNat pow)
    return arr

/-- Generate all possible sums by adding one array at a time. --/
def genSums (arrs : Array (Array Int)) : Array Int :=
  Id.run do
    let mut sums : Array Int := #[0]
    for arr in arrs do
      let mut newSums : Array Int := Array.mkEmpty (sums.size * arr.size)
      for s in sums do
        for v in arr do
          newSums := newSums.push (s + v)
      sums := newSums
    return sums

/-- Lower bound of `x` in a sorted array. --/
def lowerBound (arr : Array Int) (x : Int) : Nat :=
  let rec go (l r : Nat) :=
    if h : l < r then
      let m := (l + r) / 2
      if arr.get! m < x then go (m + 1) r else go l m
    else l
  go 0 arr.size

/-- Upper bound of `x` in a sorted array. --/
def upperBound (arr : Array Int) (x : Int) : Nat :=
  let rec go (l r : Nat) :=
    if h : l < r then
      let m := (l + r) / 2
      if arr.get! m <= x then go (m + 1) r else go l m
    else l
  go 0 arr.size

/-- Count solutions using meet-in-the-middle. --/
def countSolutions (ks : Array Int) (ps : Array Nat) (m : Nat) : Int :=
  Id.run do
    let n := ks.size
    let half := n / 2
    let mut arrs : Array (Array Int) := Array.mkEmpty n
    for i in [0:n] do
      arrs := arrs.push (precompute (ks.get! i) (ps.get! i) m)
    let left := arrs.extract 0 half
    let right := arrs.extract half n
    let sumsLeft := (genSums left).qsort (fun a b => a < b)
    let sumsRight := genSums right
    let mut total : Int := 0
    for s in sumsRight do
      let l := lowerBound sumsLeft (-s)
      let r := upperBound sumsLeft (-s)
      total := total + Int.ofNat (r - l)
    return total

/-- Main entry point: parse input and print counts. --/
def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let mut idx := 0
  let t := (tokens.get! idx).toNat!
  idx := idx + 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := (tokens.get! idx).toNat!
    idx := idx + 1
    let m := (tokens.get! idx).toNat!
    idx := idx + 1
    let mut ks : Array Int := Array.mkEmpty n
    let mut ps : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      let k := (tokens.get! idx).toInt!
      let p := (tokens.get! (idx+1)).toNat!
      idx := idx + 2
      ks := ks.push k
      ps := ps.push p
    let ans := countSolutions ks ps m
    outs := outs.push (toString ans)
  IO.println (String.intercalate "\n" outs)
