/- Solution for SPOJ POTHOLE - Potholers
https://www.spoj.com/problems/POTHOLE/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- BFS from a start node returning all reachable nodes. --/
partial def bfsFrom (start : Nat) (adj : Array (List Nat)) : Std.HashSet Nat :=
  let rec loop (q : List Nat) (vis : Std.HashSet Nat) : Std.HashSet Nat :=
    match q with
    | [] => vis
    | v :: qs =>
      if vis.contains v then
        loop qs vis
      else
        let vis := vis.insert v
        let neigh := adj.get! v
        loop (qs ++ neigh) vis
  loop [start] Std.HashSet.empty

/-- Try to find an augmenting path from left node `u`. --/
partial def augment (u : Nat) (adj : Array (List Nat))
    (matchR : Array (Option Nat)) (vis : Array Bool) : Option (Array (Option Nat)) :=
  let rec go (edges : List Nat) (matchR : Array (Option Nat)) (vis : Array Bool) :=
    match edges with
    | [] => none
    | v :: vs =>
      if vis.get! v then
        go vs matchR vis
      else
        let vis := vis.set! v true
        match matchR.get! v with
        | none => some (matchR.set! v (some u))
        | some u2 =>
          match augment u2 adj matchR vis with
          | some m2 => some (m2.set! v (some u))
          | none    => go vs matchR vis
  go (adj.get! u) matchR vis

/-- Compute maximum bipartite matching. --/
def maxMatching (adj : Array (List Nat)) (rsize : Nat) : Nat := Id.run do
  let mut matchR : Array (Option Nat) := Array.mkArray rsize none
  let mut res : Nat := 0
  for u in [0:adj.size] do
    let vis := Array.mkArray rsize false
    match augment u adj matchR vis with
    | some m2 =>
      matchR := m2
      res := res + 1
    | none => pure ()
  return res

/-- Solve a single test case starting at index `start` in `data`.
    Returns output string and new index. -/
partial def solveCase (data : Array Nat) (start : Nat) : (String × Nat) := Id.run do
  let n := data.get! start
  let mut idx := start + 1
  let mut adj : Array (List Nat) := Array.replicate (n+1) []
  for i in [1:n] do
    let m := data[idx]!; idx := idx + 1
    let mut lst : List Nat := []
    for _ in [0:m] do
      let v := data[idx]!; idx := idx + 1
      lst := v :: lst
    adj := adj.set! i lst.reverse
  -- start neighbors
  let starts := adj.get! 1
  -- targets: nodes with edge to n
  let mut targets : List Nat := []
  for i in [1:n] do
    if (adj.get! i).contains n then
      targets := i :: targets
  let tArr := targets.reverse.toArray
  let sArr := starts.toArray
  let mut bip : Array (List Nat) := Array.replicate sArr.size []
  for i in [0:sArr.size] do
    let u := sArr[i]!
    let reach := bfsFrom u adj
    for j in [0:tArr.size] do
      let v := tArr[j]!
      if reach.contains v then
        bip := bip.modify i (fun lst => j :: lst)
  let ans := maxMatching bip tArr.size
  (toString ans, idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (out, idx') := solveCase data idx
    IO.println out
    idx := idx'
