/- Solution for SPOJ QTREE2 - Query on a tree II
https://www.spoj.com/problems/QTREE2/
-/

import Std
open Std

def MAXLOG : Nat := 15

-- split input into tokens
def parseTokens (s : String) : Array String :=
  s.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
   |>.filter (fun t => t ≠ "")
   |> List.toArray

-- lift node u by k steps using binary lifting table
def lift (up : Array (Array Nat)) (u0 k0 : Nat) : Nat :=
  Id.run do
    let mut u := u0
    let mut k := k0
    let mut j := 0
    while k > 0 do
      if k % 2 = 1 then
        u := (up[u]!)[j]!
      k := k / 2
      j := j + 1
    return u

-- build ancestor table, depth and distance arrays
def buildLCA (n : Nat) (adj : Array (Array (Nat × Nat))) :
    IO (Array (Array Nat) × Array Nat × Array Nat) := do
  let upRef ← IO.mkRef (Array.replicate (n+1) (Array.replicate MAXLOG 0))
  let depthRef ← IO.mkRef (Array.replicate (n+1) 0)
  let distRef ← IO.mkRef (Array.replicate (n+1) 0)
  -- iterative DFS using a list as stack
  let mut stack : List (Nat × Nat × Nat × Nat) := [(1, 0, 0, 0)]
  while stack ≠ [] do
    let (u, p, d, len) := stack.head!
    stack := stack.tail!
    let depth ← depthRef.get
    depthRef.set (depth.set! u d)
    let dist ← distRef.get
    distRef.set (dist.set! u len)
    let up ← upRef.get
    upRef.set (up.modify u (fun row => row.set! 0 p))
    for (v, w) in adj[u]! do
      if v ≠ p then
        stack := (v, u, d+1, len + w) :: stack
  for j in [1:MAXLOG] do
    for v in [1:n+1] do
      let up ← upRef.get
      let mid := (up[v]!)[j-1]!
      let anc := if mid = 0 then 0 else (up[mid]!)[j-1]!
      upRef.set (up.modify v (fun row => row.set! j anc))
  let up ← upRef.get
  let depth ← depthRef.get
  let dist ← distRef.get
  return (up, depth, dist)

-- lowest common ancestor
def lca (up : Array (Array Nat)) (depth : Array Nat) (a b : Nat) : Nat :=
  Id.run do
    let mut u := a
    let mut v := b
    if depth[u]! < depth[v]! then
      let tmp := u; u := v; v := tmp
    let mut diff := depth[u]! - depth[v]!
    let mut bit := 0
    while diff > 0 do
      if diff % 2 = 1 then
        u := (up[u]!)[bit]!
      diff := diff / 2
      bit := bit + 1
    if u = v then
      return u
    let mut j := MAXLOG
    while j > 0 do
      j := j - 1
      let pu := (up[u]!)[j]!
      let pv := (up[v]!)[j]!
      if pu ≠ pv then
        u := pu
        v := pv
    return (up[u]!)[0]!

def distance (up : Array (Array Nat)) (depth : Array Nat) (dist : Array Nat)
    (a b : Nat) : Nat :=
  let l := lca up depth a b
  dist[a]! + dist[b]! - 2 * dist[l]!

-- k-th node on path from a to b (1-indexed)
def kthNode (up : Array (Array Nat)) (depth : Array Nat) (a b k : Nat) : Nat :=
  let l := lca up depth a b
  let d1 := depth[a]! - depth[l]!
  if k - 1 ≤ d1 then
    lift up a (k-1)
  else
    let k2 := k - d1 - 1
    let d2 := depth[b]! - depth[l]!
    lift up b (d2 - k2)

def solve (toks : Array String) : IO Unit := do
  let mut idx := 0
  let t := toks[idx]! |>.toNat!
  idx := idx + 1
  for _ in [0:t] do
    let n := toks[idx]! |>.toNat!
    idx := idx + 1
    let mut adj : Array (Array (Nat × Nat)) := Array.replicate (n+1) #[]
    for _ in [0:n-1] do
      let a := toks[idx]! |>.toNat!
      let b := toks[idx+1]! |>.toNat!
      let c := toks[idx+2]! |>.toNat!
      adj := adj.modify a (fun arr => arr.push (b, c))
      adj := adj.modify b (fun arr => arr.push (a, c))
      idx := idx + 3
    let (up, depth, dist) ← buildLCA n adj
    let mut done := false
    while !done do
      let cmd := toks[idx]!
      idx := idx + 1
      if cmd = "DONE" then
        done := true
      else if cmd = "DIST" then
        let a := toks[idx]! |>.toNat!
        let b := toks[idx+1]! |>.toNat!
        idx := idx + 2
        let ans := distance up depth dist a b
        IO.println ans
      else -- KTH
        let a := toks[idx]! |>.toNat!
        let b := toks[idx+1]! |>.toNat!
        let k := toks[idx+2]! |>.toNat!
        idx := idx + 3
        let ans := kthNode up depth a b k
        IO.println ans
    IO.println ""

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := parseTokens data
  solve toks
