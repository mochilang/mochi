/- Solution for SPOJ TPERML - Printing Subsequent Permutations by Index
https://www.spoj.com/problems/TPERML/
-/

import Std
open Std

/-- remove the element at position `n` from list `xs`, returning the element and the remaining list. -/
def removeNth : List Nat → Nat → Nat × List Nat
| [], _ => (0, [])
| x::xs, 0 => (x, xs)
| x::xs, n+1 =>
  let (y, rest) := removeNth xs n
  (y, x::rest)

/-- compute the factoradic representation digits for given `n` and `idx`.
    returns an array `d` with `d[i]` = digit for (i+1)! place, i from 0..n-1. -/
partial def factoradic (n idx : Nat) : Array Nat :=
  Id.run do
    let mut digits : Array Nat := #[]
    let mut i := 1
    let mut k := idx
    while i ≤ n do
      digits := digits.push (k % i)
      k := k / i
      i := i + 1
    return digits

/-- build the permutation of [1..n] corresponding to index `idx` in lexicographic order. -/
partial def nthPermutation (n idx : Nat) : Array Nat :=
  let digits := factoradic n idx
  let rec build (i : Nat) (avail : List Nat) (acc : Array Nat) : Array Nat :=
    match i with
    | 0 => acc
    | i+1 =>
      let d := digits.get! i
      let (elem, rest) := removeNth avail d
      build i rest (acc.push elem)
  let avail := (List.range n).map (·+1)
  build n avail #[]

/-- compute next lexicographic permutation; wraps to first permutation when at the last. -/
partial def nextPermutation (arr : Array Nat) : Array Nat :=
  Id.run do
    let mut a := arr
    let n := a.size
    if n = 0 then return a
    let mut i := n - 1
    while i > 0 && a.get! (i-1) ≥ a.get! i do
      i := i - 1
    if i = 0 then
      return ((List.range n).map (·+1)).toArray
    let pivotIdx := i - 1
    let pivot := a.get! pivotIdx
    let mut j := n - 1
    while a.get! j ≤ pivot do
      j := j - 1
    a := a.swap! pivotIdx j
    let mut l := i
    let mut r := n - 1
    while l < r do
      a := a.swap! l r
      l := l + 1
      r := r - 1
    return a

partial def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks[0]!.toNat!
    let mut idxTok := 1
    let mut res : Array String := #[]
    for ti in [0:t] do
      let n := toks[idxTok]!.toNat!
      let index := toks[idxTok+1]!.toNat!
      let m := toks[idxTok+2]!.toNat!
      idxTok := idxTok + 3
      let mut perm := nthPermutation n index
      for _ in [0:m] do
        let line := String.intercalate " " (perm.toList.map toString)
        res := res.push line
        perm := nextPermutation perm
      if ti + 1 < t then
        res := res.push ""
    return res

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  for line in solve toks do
    IO.println line
