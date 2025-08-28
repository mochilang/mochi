/- Solution for SPOJ MDST - Minimum Diameter Spanning Tree
https://www.spoj.com/problems/MDST/
-/

import Std
open Std

/-- Breadth-first search returning distances from start. --/
private def bfs (adj : Array (Array Nat)) (start : Nat) : Array Nat := Id.run do
  let n := adj.size
  let mut dist : Array (Option Nat) := Array.mkArray n none
  let mut q : Array Nat := #[start]
  let mut head : Nat := 0
  dist := dist.set! start (some 0)
  while head < q.size do
    let v := q[head]!
    head := head + 1
    let dv := (dist[v]!).getD 0
    for w in adj[v]! do
      if (dist[w]!).isNone then
        dist := dist.set! w (some (dv + 1))
        q := q.push w
  return dist.map (fun d => d.getD 0)

/-- Compute minimum possible diameter of spanning tree for given graph. --/
private def solveCase (adj : Array (Array Nat)) : Nat := Id.run do
  let n := adj.size
  let mut ecc : Array Nat := Array.mkArray n 0
  for v in [0:n] do
    let dist := bfs adj v
    let m := dist.foldl Nat.max 0
    ecc := ecc.set! v m
  let mut r := ecc[0]!
  for e in ecc do
    r := Nat.min r e
  let centers := (List.range n).filter (fun i => ecc[i]! = r)
  let mut set : Std.HashSet Nat := {}
  for c in centers do
    set := set.insert c
  let mut pair := false
  for u in centers do
    if !pair then
      for v in adj[u]! do
        if set.contains v then
          pair := true
  return if pair then 2 * r - 1 else 2 * r

/-- Read all integers from stdin. --/
private def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Main program: parse input and output answers. --/
def main : IO Unit := do
  let data ← readInts
  let t := data[0]!
  let mut idx := 1
  for _ in [0:t] do
    let n := data[idx]!; idx := idx + 1
    let mut adj : Array (Array Nat) := Array.mkArray n #[]
    for _ in [0:n] do
      let v := data[idx]! - 1; idx := idx + 1
      let m := data[idx]!; idx := idx + 1
      let mut neighbors : Array Nat := #[]
      for _ in [0:m] do
        neighbors := neighbors.push (data[idx]! - 1)
        idx := idx + 1
      adj := adj.set! v neighbors
    IO.println (solveCase adj)
