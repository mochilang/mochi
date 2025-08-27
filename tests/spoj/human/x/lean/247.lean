/- Solution for SPOJ CHOCOLA - Chocolate
https://www.spoj.com/problems/CHOCOLA/
-/

import Std
open Std

/-- Compute minimal breaking cost given vertical and horizontal cut costs. -/
def minCost (xs ys : List Nat) : Nat :=
  Id.run do
    let mut xs := xs.toArray
    let mut ys := ys.toArray
    xs := xs.qsort (fun a b => b < a)
    ys := ys.qsort (fun a b => b < a)
    let mut i := 0
    let mut j := 0
    let mut h := 1
    let mut v := 1
    let mut cost := 0
    while i < xs.size ∧ j < ys.size do
      if xs[i]! > ys[j]! then
        cost := cost + xs[i]! * h
        v := v + 1
        i := i + 1
      else
        cost := cost + ys[j]! * v
        h := h + 1
        j := j + 1
    while i < xs.size do
      cost := cost + xs[i]! * h
      i := i + 1
    while j < ys.size do
      cost := cost + ys[j]! * v
      j := j + 1
    return cost

partial def solveCases : List Nat -> Nat -> List Nat -> List Nat
| _, 0, acc => acc.reverse
| nums, Nat.succ t, acc =>
  match nums with
  | m :: n :: rest =>
      let xs := rest.take (m-1)
      let rest := rest.drop (m-1)
      let ys := rest.take (n-1)
      let rest := rest.drop (n-1)
      let ans := minCost xs ys
      solveCases rest t (ans :: acc)
  | _ => acc.reverse

/-- Entry point parsing tokens and running all test cases. -/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
  let nums := toks.map (·.toNat!)
  match nums with
  | [] => pure ()
  | t :: rest =>
      let res := solveCases rest t []
      let strs := res.map (fun n => s!"{n}")
      IO.println (String.intercalate "\n" strs)
