/- Solution for SPOJ COVER - K-path cover
https://www.spoj.com/problems/COVER/
-/
import Std
open Std

structure Edge where
  to : Nat
  rev : Nat
  cap : Int
  cost : Int
  deriving Inhabited

def addEdge (g : Array (Array Edge)) (u v : Nat) (cap cost : Int) : Array (Array Edge) :=
  let e1 : Edge := { to := v, rev := (g[v]!).size, cap := cap, cost := cost }
  let e2 : Edge := { to := u, rev := (g[u]!).size, cap := 0, cost := -cost }
  let g := g.set! u ((g[u]!).push e1)
  g.set! v ((g[v]!).push e2)

partial def spfa (g : Array (Array Edge)) (s t : Nat)
  : Option (Int × Array (Nat × Nat)) :=
  Id.run do
    let n := g.size
    let inf : Int := (1 <<< 60)
    let mut dist : Array Int := Array.replicate n inf
    dist := dist.set! s 0
    let mut inQ : Array Bool := Array.replicate n false
    inQ := inQ.set! s true
    let mut prev : Array (Nat × Nat) := Array.replicate n (0, 0)
    let mut q : Array Nat := #[s]
    let mut qi := 0
    while qi < q.size do
      let u := q[qi]!
      qi := qi + 1
      inQ := inQ.set! u false
      let edges := g[u]!
      for i in [0:edges.size] do
        let e := edges[i]!
        if e.cap > 0 then
          let nd := dist[u]! + e.cost
          if nd < dist[e.to]! then
            dist := dist.set! e.to nd
            prev := prev.set! e.to (u, i)
            if !inQ[e.to]! then
              q := q.push e.to
              inQ := inQ.set! e.to true
    if dist[t]! == inf then
      none
    else
      some (dist[t]!, prev)

partial def minCostFlow (g0 : Array (Array Edge)) (s t k : Nat)
  : Option Int :=
  Id.run do
    let mut g := g0
    let mut total : Int := 0
    for _ in [0:k] do
      match spfa g s t with
      | none => return none
      | some (d, prev) =>
        -- find minimum residual capacity along the path
        let mut f : Int := (1 <<< 60)
        let mut v := t
        while v ≠ s do
          let (u, idx) := prev[v]!
          let e := g[u]![idx]!
          if e.cap < f then
            f := e.cap
          v := u
        -- update capacities
        v := t
        while v ≠ s do
          let (u, idx) := prev[v]!
          let e := g[u]![idx]!
          let rowu := g[u]!
          let e1 := rowu[idx]!
          let rowu := rowu.set! idx {e1 with cap := e1.cap - f}
          g := g.set! u rowu
          let rowv := g[e.to]!
          let e2 := rowv[e.rev]!
          let rowv := rowv.set! e.rev {e2 with cap := e2.cap + f}
          g := g.set! e.to rowv
          v := u
        total := total + d * f
    some total

partial def solveCase (k n m : Nat) (costs : Array Int)
    (edges : Array (Nat × Nat)) : Option Int :=
  let size := 2 * n + 2
  let s := 2 * n
  let t := 2 * n + 1
  let g0 : Array (Array Edge) := Array.replicate size #[]
  let g := Id.run do
    let mut g := g0
    for i in [0:n] do
      g := addEdge g s i 1 0
    for i in [0:n] do
      g := addEdge g (i + n) t 1 0
    for (u, v) in edges do
      let cu := costs[u - 1]!
      let cv := costs[v - 1]!
      g := addEdge g (u - 1) (v - 1 + n) 1 (cu + cv)
    return g
  minCostFlow g s t k

partial def parseInput : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |> List.filter (fun s => s ≠ "")
                |> List.toArray
  if toks.size = 0 then
    return ()
  let mut idx := 0
  let cases := toks[idx]!.toNat!
  idx := idx + 1
  for _ in [0:cases] do
    let k := toks[idx]!.toNat!; idx := idx + 1
    let n := toks[idx]!.toNat!; idx := idx + 1
    let m := toks[idx]!.toNat!; idx := idx + 1
    let mut costs : Array Int := Array.mkEmpty n
    for _ in [0:n] do
      costs := costs.push toks[idx]!.toInt!
      idx := idx + 1
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let u := toks[idx]!.toNat!
      let v := toks[idx+1]!.toNat!
      idx := idx + 2
      edges := edges.push (u, v)
    match solveCase k n m costs edges with
    | some ans => IO.println s!"{ans}"
    | none => IO.println "NONE"

def main : IO Unit := parseInput
