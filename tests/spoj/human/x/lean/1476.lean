/- Solution for SPOJ PROFIT - Maximum Profit
https://www.spoj.com/problems/PROFIT/
-/

import Std
open Std

structure Edge where
  to : Nat
  rev : Nat
  cap : Nat

deriving Inhabited

private def addEdge (g : Array (Array Edge)) (u v cap : Nat) : Array (Array Edge) :=
  Id.run do
    let mut g := g
    let rowU := g[u]!
    let rowV := g[v]!
    let revU := rowV.size
    let revV := rowU.size
    g := g.set! u (rowU.push {to := v, rev := revU, cap := cap})
    g := g.set! v (rowV.push {to := u, rev := revV, cap := 0})
    return g

private def maxFlow (gInit : Array (Array Edge)) (s t : Nat) : Nat :=
  Id.run do
    let n := gInit.size
    let mut g := gInit
    let mut total := 0
    let inf := 1000000000
    while true do
      let mut parent : Array (Option (Nat × Nat)) := Array.replicate n none
      parent := parent.set! s (some (s,0))
      let mut q : Array Nat := #[s]
      let mut qi := 0
      while qi < q.size && parent[t]! = none do
        let u := q[qi]!; qi := qi + 1
        for i in [0:g[u]!.size] do
          let e := g[u]![i]!
          if e.cap > 0 && parent[e.to]! = none then
            parent := parent.set! e.to (some (u,i))
            q := q.push e.to
      if parent[t]! = none then
        break
      let mut f := inf
      let mut v := t
      while v ≠ s do
        match parent[v]! with
        | some (u,i) =>
            let e := g[u]![i]!
            f := Nat.min f e.cap
            v := u
        | none => break
      v := t
      while v ≠ s do
        match parent[v]! with
        | some (u,i) =>
            let e := g[u]![i]!
            let rowU := g[u]!
            let rowU := rowU.set! i {e with cap := e.cap - f}
            g := g.set! u rowU
            let rowV := g[e.to]!
            let revEdge := rowV[e.rev]!
            g := g.set! e.to (rowV.set! e.rev {revEdge with cap := revEdge.cap + f})
            v := u
        | none => break
      total := total + f
    return total

private def maxProfit (n m : Nat) (cost : Array Nat) (edges : Array (Nat × Nat × Nat)) : Nat :=
  let totalNodes := n + m + 2
  let src := 0
  let sink := totalNodes - 1
  let inf := 1000000000
  Id.run do
    let mut g : Array (Array Edge) := Array.replicate totalNodes Array.empty
    let mut total := 0
    for j in [0:m] do
      let (a,b,c) := edges[j]!
      total := total + c
      let pj := 1 + n + j
      g := addEdge g src pj c
      g := addEdge g pj (1 + a) inf
      g := addEdge g pj (1 + b) inf
    for i in [0:n] do
      g := addEdge g (1 + i) sink cost[i]!
    let flow := maxFlow g src sink
    return total - flow

partial def solve (arr : Array Nat) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := arr[idx]!
    let m := arr[idx+1]!
    let mut id := idx + 2
    let mut cost : Array Nat := Array.replicate n 0
    for i in [0:n] do
      cost := cost.set! i arr[id]!; id := id + 1
    let mut edges : Array (Nat × Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let a := arr[id]! - 1
      let b := arr[id+1]! - 1
      let c := arr[id+2]!
      edges := edges.push (a,b,c)
      id := id + 3
    let ans := maxProfit n m cost edges
    IO.println ans
    solve arr id (t-1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let arr := toks.map String.toNat!
    let t := arr[0]!
    solve arr 1 t
