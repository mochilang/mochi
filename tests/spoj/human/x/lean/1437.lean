/- Solution for SPOJ PT07Z - Longest path in a tree
https://www.spoj.com/problems/PT07Z/
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

/-- BFS from a start node returning the farthest node and its distance. --/
partial def bfsFar (start : Nat) (adj : Array (List Nat)) : Nat × Nat :=
  let rec loop (q : List (Nat × Nat)) (vis : Std.HashSet Nat) (far : Nat × Nat) : Nat × Nat :=
    match q with
    | [] => far
    | (v,d) :: qs =>
        if vis.contains v then
          loop qs vis far
        else
          let vis := vis.insert v
          let far := if d > far.snd then (v,d) else far
          let neigh := (adj.get! v).map (fun u => (u, d+1))
          loop (qs ++ neigh) vis far
  loop [(start,0)] Std.HashSet.empty (start,0)

def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then
    return
  let n := data[0]!
  let edges := n - 1
  let mut idx := 1
  let mut adj : Array (List Nat) := Array.replicate (n+1) []
  for _ in [0:edges] do
    let u := data[idx]!; let v := data[idx+1]!
    idx := idx + 2
    adj := adj.modify u (fun lst => v :: lst)
    adj := adj.modify v (fun lst => u :: lst)
  let (s, _) := bfsFar 1 adj
  let (_, dist) := bfsFar s adj
  IO.println dist
