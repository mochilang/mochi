/- Solution for SPOJ CABLETV - Cable TV Network
https://www.spoj.com/problems/CABLETV/
-/

import Std
open Std

-- BFS to check if graph is connected
private def isConnected (adj : Array (Array Bool)) (n : Nat) : Bool :=
  if n = 0 then true
  else
    Id.run do
      let mut vis : Array Bool := Array.mkArray n false
      let mut q : Array Nat := #[0]
      vis := vis.set! 0 true
      let mut qi := 0
      while qi < q.size do
        let u := q[qi]!
        qi := qi + 1
        for v in [0:n] do
          if !vis[v]! && adj[u]![v]! then
            vis := vis.set! v true
            q := q.push v
      let mut all := true
      for i in [0:n] do
        all := all && vis[i]!
      return all

-- Check if graph is complete
private def isComplete (adj : Array (Array Bool)) (n : Nat) : Bool :=
  Id.run do
    let mut ok := true
    for i in [0:n] do
      for j in [i+1:n] do
        if !adj[i]![j]! then
          ok := false
    return ok

-- Build capacity matrix for vertex split graph between s and t
private def buildCap (adj : Array (Array Bool)) (n s t : Nat) : Array (Array Nat) :=
  Id.run do
    let size := 2 * n
    let inf := n
    let mut cap : Array (Array Nat) := Array.mkArray size (Array.mkArray size 0)
    -- split vertices
    for v in [0:n] do
      let c := if v = s || v = t then inf else 1
      let row := cap[2*v]!
      cap := cap.set! (2*v) (row.set! (2*v+1) c)
    -- edges
    for u in [0:n] do
      for v in [0:n] do
        if adj[u]![v]! then
          let row := cap[2*u+1]!
          cap := cap.set! (2*u+1) (row.set! (2*v) inf)
    return cap

-- Edmonds-Karp max flow
private def maxFlow (capInit : Array (Array Nat)) (s t : Nat) : Nat :=
  Id.run do
    let n := capInit.size
    let mut cap := capInit
    let mut total := 0
    let inf := 1000000 -- big enough
    while true do
      let mut parent : Array (Option Nat) := Array.mkArray n none
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
        let row := cap[u]!
        let c := row[v]!
        f := Nat.min f c
        v := u
      total := total + f
      v := t
      while v ≠ s do
        let u := (parent[v]!).get!
        let rowu := cap[u]!
        cap := cap.set! u (rowu.set! v (rowu[v]! - f))
        let rowv := cap[v]!
        cap := cap.set! v (rowv.set! u (rowv[u]! + f))
        v := u
    return total

-- Compute safety factor
private def safety (n : Nat) (adj : Array (Array Bool)) : Nat :=
  if n ≤ 1 then n
  else if !isConnected adj n then 0
  else if isComplete adj n then n
  else
    Id.run do
      let mut ans := n
      for s in [0:n] do
        for t in [s+1:n] do
          let cap := buildCap adj n s t
          let k := maxFlow cap (2*s+1) (2*t)
          if k < ans then ans := k
      return ans

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := toks[idx]!.toNat!
    let m := toks[idx+1]!.toNat!
    let mut adj : Array (Array Bool) := Array.mkArray n (Array.mkArray n false)
    let mut i := 0
    let mut id := idx + 2
    while i < m do
      let tok := toks[id]!
      let trimmed := (tok.drop 1).dropRight 1
      let parts := (trimmed.splitOn ",").toArray
      let u := parts[0]!.toNat!
      let v := parts[1]!.toNat!
      let rowu := adj[u]!
      adj := adj.set! u (rowu.set! v true)
      let rowv := adj[v]!
      adj := adj.set! v (rowv.set! u true)
      i := i + 1
      id := id + 1
    let ans := safety n adj
    IO.println ans
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
