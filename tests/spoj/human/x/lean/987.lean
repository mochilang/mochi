/- Solution for SPOJ MOBILE - Mobile
https://www.spoj.com/problems/MOBILE/
-/

import Std
open Std

partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, parent') := find parent px
    (r, parent'.set! x r)

def union (parent : Array Nat) (a b : Nat) : Array Nat :=
  let (ra, parent1) := find parent a
  let (rb, parent2) := find parent1 b
  if ra = rb then parent2 else parent2.set! rb ra

/-- Check ultrametric property using union-find buckets. -/
def check (n : Nat) (buckets : Array (Array (Nat × Nat))) : Bool :=
  Id.run do
    let mut parent : Array Nat := Array.mkArray n 0
    for i in [0:n] do
      parent := parent.set! i i
    for d in [1:2048] do
      -- verify edges of distance d do not connect existing components
      for (i,j) in buckets[d]! do
        let (ri, parent1) := find parent i
        parent := parent1
        let (rj, parent2) := find parent j
        parent := parent2
        if ri = rj then
          return false
      -- merge edges of distance d
      for (i,j) in buckets[d]! do
        parent := union parent i j
    return true

partial def process (tokens : Array String) (idx : Nat) : IO Unit := do
  let n := (tokens.get! idx).toNat!
  if n = 0 then
    pure ()
  else
    let mut idx := idx + 1
    let mut mat : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    let mut buckets : Array (Array (Nat × Nat)) := Array.mkArray 2048 (#[] : Array (Nat × Nat))
    let mut ok := true
    for i in [0:n] do
      for j in [0:n] do
        let v := (tokens.get! idx).toNat!
        idx := idx + 1
        mat := mat.modify i (fun row => row.set! j v)
        if i = j then
          if v ≠ 0 then ok := false
        else
          if v = 0 || v ≥ 2048 then ok := false
          if j < i then
            if v ≠ (mat.get! j).get! i then ok := false
          else
            buckets := buckets.modify v (fun arr => arr.push (i,j))
    let ans := ok && check n buckets
    IO.println (if ans then "true" else "false")
    process tokens idx

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  process tokens 0
