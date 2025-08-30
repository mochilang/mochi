/- Solution for SPOJ COCONUTS - Coconuts
https://www.spoj.com/problems/COCONUTS/
-/

import Std
open Std

private def maxFlow (capInit : Array (Array Nat)) (s t : Nat) : Nat :=
  Id.run do
    let n := capInit.size
    let mut cap := capInit
    let mut total := 0
    let inf := 1000000
    while true do
      let mut parent : Array (Option Nat) := Array.replicate n none
      parent := parent.set! s (some s)
      let mut q : Array Nat := #[s]
      let mut qi := 0
      while qi < q.size && parent[t]! = none do
        let u := q[qi]!
        qi := qi + 1
        for v in [0:n] do
          if parent[v]! = none && cap[u]![v]! > 0 then
            parent := parent.set! v (some u)
            q := q.push v
      if parent[t]! = none then
        break
      let mut f := inf
      let mut v := t
      while v ≠ s do
        let u := (parent[v]!).get!
        let c := cap[u]![v]!
        f := Nat.min f c
        v := u
      v := t
      while v ≠ s do
        let u := (parent[v]!).get!
        let rowu := cap[u]!
        cap := cap.set! u (rowu.set! v (rowu[v]! - f))
        let rowv := cap[v]!
        cap := cap.set! v (rowv.set! u (rowv[u]! + f))
        v := u
      total := total + f
    return total

partial def solve (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  let n := toks[idx]!.toNat!
  let m := toks[idx+1]!.toNat!
  if n = 0 && m = 0 then
    acc.reverse
  else
    let mut beliefs : Array Nat := Array.mkArray n 0
    for i in [0:n] do
      beliefs := beliefs.set! i (toks[idx+2+i]!.toNat!)
    let mut id := idx + 2 + n
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let u := toks[id]!.toNat! - 1
      let v := toks[id+1]!.toNat! - 1
      edges := edges.push (u, v)
      id := id + 2
    let size := n + 2
    let src := n
    let sink := n + 1
    let mut cap : Array (Array Nat) := Array.replicate size (Array.replicate size 0)
    for i in [0:n] do
      if beliefs[i]! = 1 then
        let row := cap[src]!
        cap := cap.set! src (row.set! i 1)
      else
        let row := cap[i]!
        cap := cap.set! i (row.set! sink 1)
    for (u,v) in edges do
      let rowu := cap[u]!
      cap := cap.set! u (rowu.set! v (rowu[v]! + 1))
      let rowv := cap[v]!
      cap := cap.set! v (rowv.set! u (rowv[u]! + 1))
    let res := maxFlow cap src sink
    solve toks id (toString res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let outs := solve toks 0 []
  for line in outs do
    IO.println line
