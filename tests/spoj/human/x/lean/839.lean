/- Solution for SPOJ OPTM - Optimal Marks
https://www.spoj.com/problems/OPTM/
-/

import Std
open Std

-- Edmonds-Karp max flow.  Returns the vertices reachable from `s` in the
-- residual network of a minimum cut.
private def minCutReachable (capInit : Array (Array Nat)) (s t : Nat) : Array Bool :=
  Id.run do
    let n := capInit.size
    let mut cap := capInit
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
    -- BFS in residual graph to find reachable vertices
    let mut vis : Array Bool := Array.replicate n false
    let mut q : Array Nat := #[s]
    let mut qi := 0
    vis := vis.set! s true
    while qi < q.size do
      let u := q[qi]!
      qi := qi + 1
      for v in [0:n] do
        if !vis[v]! && cap[u]![v]! > 0 then
          vis := vis.set! v true
          q := q.push v
    return vis

-- Compute optimal marks for one test case
private def computeMarks (n : Nat) (edges : Array (Nat × Nat)) (known : Array (Nat × Nat)) : Array Nat :=
  Id.run do
    let mut mark : Array Nat := Array.replicate n 0
    for (u,p) in known do
      mark := mark.set! u p
    for b in [0:31] do
      let size := n + 2
      let src := n
      let sink := n + 1
      let inf := 1000000
      let mut cap : Array (Array Nat) := Array.replicate size (Array.replicate size 0)
      for (u,v) in edges do
        let rowu := cap[u]!
        cap := cap.set! u (rowu.set! v (rowu[v]! + 1))
        let rowv := cap[v]!
        cap := cap.set! v (rowv.set! u (rowv[u]! + 1))
      for (u,p) in known do
        if ((p >>> b) &&& 1) = 1 then
          let row := cap[src]!
          cap := cap.set! src (row.set! u inf)
        else
          let row := cap[u]!
          cap := cap.set! u (row.set! sink inf)
      let reach := minCutReachable cap src sink
      for i in [0:n] do
        if reach[i]! then
          mark := mark.set! i (mark[i]! ||| (1 <<< b))
    return mark

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := toks[idx]!.toNat!
    let m := toks[idx+1]!.toNat!
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    let mut id := idx + 2
    for _ in [0:m] do
      let u := toks[id]!.toNat! - 1
      let v := toks[id+1]!.toNat! - 1
      edges := edges.push (u, v)
      id := id + 2
    let k := toks[id]!.toNat!
    id := id + 1
    let mut known : Array (Nat × Nat) := Array.mkEmpty k
    for _ in [0:k] do
      let u := toks[id]!.toNat! - 1
      let p := toks[id+1]!.toNat!
      known := known.push (u, p)
      id := id + 2
    let ans := computeMarks n edges known
    for i in [0:n] do
      IO.println (toString (ans[i]!))
    solve toks id (t-1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    solve toks 1 t
