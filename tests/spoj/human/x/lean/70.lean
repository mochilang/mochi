/- Solution for SPOJ RELATS1 - Relations
https://www.spoj.com/problems/RELATS1/
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

/-- Solve a single test case. Returns `some k` or `none` if impossible. -/
def solveCase (n : Nat) (edges : Array (Nat × Nat × Int)) : Option Nat :=
  Id.run do
    -- union-find for equality edges
    let mut parent := Array.replicate n 0
    for i in [0:n] do
      parent := parent.set! i i
    for (u, v, r) in edges do
      if r == 0 then
        parent := union parent u v
    -- compress components
    let mut parent2 := parent
    let mut ids := Array.replicate n 0
    let mut compOf := Array.replicate n n -- sentinel n means unassigned
    let mut comp := 0
    for i in [0:n] do
      let (root, parent') := find parent2 i
      parent2 := parent'
      let cid := compOf[root]!
      if cid = n then
        compOf := compOf.set! root comp
        ids := ids.set! i comp
        comp := comp + 1
      else
        ids := ids.set! i cid
    -- build graph of inequalities
    let mut adj := Array.replicate comp ([] : List Nat)
    let mut indeg := Array.replicate comp 0
    for (u, v, r) in edges do
      if r != 0 then
        let su := ids[u]!
        let sv := ids[v]!
        if su = sv then
          return none
        let src := if r = -1 then su else sv
        let dst := if r = -1 then sv else su
        adj := adj.set! src (dst :: adj[src]!)
        indeg := indeg.set! dst (indeg[dst]! + 1)
    -- topological order with longest path
    let mut dist := Array.replicate comp 0
    let mut queue : Array Nat := #[]
    for i in [0:comp] do
      if indeg[i]! = 0 then
        queue := queue.push i
    let mut head := 0
    let mut visited := 0
    while head < queue.size do
      let u := queue[head]!
      head := head + 1
      visited := visited + 1
      for v in adj[u]! do
        let d := dist[u]! + 1
        if dist[v]! < d then
          dist := dist.set! v d
        indeg := indeg.set! v (indeg[v]! - 1)
        if indeg[v]! = 0 then
          queue := queue.push v
    if visited != comp then
      return none
    else
      return some (dist.foldl (fun acc x => if acc < x then x else acc) 0)

/-- Entry point processing multiple cases. -/
partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let parts := (← h.getLine).trim.split (· = ' ')
    let n := (parts[0]!).toNat!
    let m := (parts[1]!).toNat!
    let tokens := (← h.getLine).trim.split (· = ' ')
    let arr := tokens.toArray
    let mut edges : Array (Nat × Nat × Int) := Array.replicate m (0,0,0)
    for i in [0:m] do
      let base := i * 3
      let u := (arr[base]!).toNat! - 1
      let v := (arr[base+1]!).toNat! - 1
      let r := (arr[base+2]!).toInt!
      edges := edges.set! i (u, v, r)
    match solveCase n edges with
    | some k => IO.println k
    | none => IO.println "NO"
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let d := (← h.getLine).trim.toNat!
  process h d
