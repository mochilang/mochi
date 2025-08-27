/- Solution for SPOJ CSTREET - Cobbled streets
https://www.spoj.com/problems/CSTREET/
-/

import Std
open Std

structure Edge where
  u v : Nat
  w : Int

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Disjoint-set find with path compression. --/
partial def find (parent : Array Nat) (x : Nat) : (Nat × Array Nat) :=
  let px := parent[x]!
  if px = x then
    (x, parent)
  else
    let (r, p') := find parent px
    (r, p'.set! x r)

/-- Compute MST total weight using Kruskal's algorithm. --/
def mst (n : Nat) (edges : Array Edge) : Int :=
  let mut parent : Array Nat := Array.mkArray (n + 1) 0
  for i in [0:n+1] do
    parent := parent.set! i i
  let mut parent2 := parent
  let mut total : Int := 0
  let sorted := edges.qsort (fun a b => a.w < b.w)
  for e in sorted do
    let (ru, p1) := find parent2 e.u
    let (rv, p2) := find p1 e.v
    parent2 := p2
    if ru ≠ rv then
      parent2 := parent2.set! rv ru
      total := total + e.w
  total

/-- Process all test cases from flattened input array. --/
def process (data : Array Int) : String :=
  let t := data[0].toNat!
  let mut idx := 1
  let mut out := ""
  for case in [0:t] do
    let p := data[idx]; idx := idx + 1
    let n := data[idx].toNat!; idx := idx + 1
    let m := data[idx].toNat!; idx := idx + 1
    let mut edges : Array Edge := Array.mkEmpty m
    for _ in [0:m] do
      let a := data[idx].toNat!; idx := idx + 1
      let b := data[idx].toNat!; idx := idx + 1
      let c := data[idx]; idx := idx + 1
      edges := edges.push {u := a, v := b, w := c}
    let cost := mst n edges * p
    out := out ++ toString cost
    if case + 1 < t then out := out ++ "\n"
  out

/-- Main: read input, solve and print outputs. --/
def main : IO Unit := do
  let data ← readInts
  IO.println (process data)
