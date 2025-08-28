/- Solution for SPOJ SUBS - String it out
https://www.spoj.com/problems/SUBS/
-/

import Std
open Std

-- count occurrences of each letter 'a'..'z'
def counts (s : List Char) : Array Nat :=
  s.foldl (fun arr c =>
    let idx := c.toNat - 'a'.toNat
    arr.modify idx (· + 1)
  ) (Array.mkArray 26 0)

-- upper bound for M based on letter frequencies
def upperBound (xs ys : List Char) : Nat :=
  if xs.length == 0 then 0 else
    let cx := counts xs
    let cy := counts ys
    let base := ys.length / xs.length
    (List.range 26).foldl (fun acc i =>
      let fx := cx.get! i
      let fy := cy.get! i
      if fx == 0 then acc else Nat.min acc (fy / fx)
    ) base

-- check if X^m is a subsequence of Y
def isSubseq (xs ys : List Char) (m : Nat) : Bool :=
  match xs with
  | [] => true
  | c::xs' =>
    let rec consume : List Char → Nat → Option (List Char)
      | ys, 0 => some ys
      | [], _ => none
      | y::ys', Nat.succ k =>
          if y = c then consume ys' k else consume ys' (Nat.succ k)
    match consume ys m with
    | some rest => isSubseq xs' rest m
    | none => false

partial def solve (x y : String) : Nat :=
  let xs := x.data
  let ys := y.data
  let ub := upperBound xs ys
  let rec bsearch (lo hi : Nat) : Nat :=
    if lo < hi then
      let mid := (lo + hi + 1) / 2
      if isSubseq xs ys mid then bsearch mid hi else bsearch lo (mid - 1)
    else lo
  bsearch 0 ub

def main : IO Unit := do
  let stdin ← IO.getStdin
  let tLine ← stdin.getLine
  let t := tLine.trim.toNat!
  for _ in List.range t do
    let x := (← stdin.getLine).trim
    let y := (← stdin.getLine).trim
    IO.println (solve x y)
