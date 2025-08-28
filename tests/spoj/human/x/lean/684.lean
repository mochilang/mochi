/- Solution for SPOJ ASSIGN4 - Another Assignment Problem
https://www.spoj.com/problems/ASSIGN4/
-/

import Std
open Std

structure Edge where
  to : Nat
  rev : Nat
  cap : Int
  cost : Int
  deriving Repr, Inhabited

def addEdge (g : Array (Array Edge)) (u v : Nat) (cap cost : Int) : Array (Array Edge) :=
  let e1 : Edge := {to := v, rev := (g[v]!).size, cap := cap, cost := cost}
  let e2 : Edge := {to := u, rev := (g[u]!).size, cap := 0, cost := -cost}
  let g := g.set! u ((g[u]!).push e1)
  let g := g.set! v ((g[v]!).push e2)
  g

def minCostFlow (g0 : Array (Array Edge)) (s t : Nat) : Int :=
  Id.run do
    let n := g0.size
    let inf : Int := 1000000000000
    let mut g := g0
    let mut pot : Array Int := Array.mkArray n 0
    let mut res : Int := 0
    while true do
      let mut dist : Array Int := Array.mkArray n inf
      let mut pv : Array Nat := Array.mkArray n 0
      let mut pe : Array Nat := Array.mkArray n 0
      let mut used : Array Bool := Array.mkArray n false
      dist := dist.set! s 0
      for _ in [0:n] do
        let mut v := n
        let mut best := inf
        for i in [0:n] do
          if !used[i]! && dist[i]! < best then
            best := dist[i]!
            v := i
        if v == n then
          break
        used := used.set! v true
        for ei in [0:(g[v]!).size] do
          let e := (g[v]!).get! ei
          if e.cap > 0 then
            let nd := dist[v]! + e.cost + pot[v]! - pot[e.to]!
            if nd < dist[e.to]! then
              dist := dist.set! e.to nd
              pv := pv.set! e.to v
              pe := pe.set! e.to ei
      if dist[t]! == inf then
        break
      for i in [0:n] do
        if dist[i]! < inf then
          pot := pot.set! i (pot[i]! + dist[i]!)
      let mut f := inf
      let mut v := t
      while v ≠ s do
        let u := pv[v]!
        let e := (g[u]!).get! (pe[v]!)
        if e.cap < f then
          f := e.cap
        v := u
      res := res + f * pot[t]!
      v := t
      while v ≠ s do
        let u := pv[v]!
        let ei := pe[v]!
        let e := (g[u]!).get! ei
        let rev := e.rev
        let rowu := g[u]!
        g := g.set! u (rowu.set! ei {e with cap := e.cap - f})
        let rowv := g[e.to]!
        let er := rowv.get! rev
        g := g.set! e.to (rowv.set! rev {er with cap := er.cap + f})
        v := u
    return res

partial def solve (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let m := (toks.get! idx).toNat!
    let n := (toks.get! (idx+1)).toNat!
    let mut idx := idx + 2
    let mut a : Array Int := Array.mkArray m 0
    for i in [0:m] do
      a := a.set! i (Int.ofNat ((toks.get! idx).toNat!))
      idx := idx + 1
    let mut b : Array Int := Array.mkArray n 0
    for j in [0:n] do
      b := b.set! j (Int.ofNat ((toks.get! idx).toNat!))
      idx := idx + 1
    let size := m + n + 2
    let source := m + n
    let sink := m + n + 1
    let mut g : Array (Array Edge) := Array.mkArray size #[]
    for i in [0:m] do
      g := addEdge g source i (a.get! i) 0
    for j in [0:n] do
      g := addEdge g (m+j) sink (b.get! j) 0
    for i in [0:m] do
      for j in [0:n] do
        let c := Int.ofNat ((toks.get! idx).toNat!)
        idx := idx + 1
        g := addEdge g i (m+j) (a.get! i) c
    let ans := minCostFlow g source sink
    solve toks idx (t-1) (toString ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
             |>.filter (fun s => s ≠ "")
             |> Array.ofList
  let t := (toks.get! 0).toNat!
  let lines := solve toks 1 t []
  for line in lines do
    IO.println line
