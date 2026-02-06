/- Solution for SPOJ PFDEP - Project File Dependencies
https://www.spoj.com/problems/PFDEP/
-/

import Std
open Std

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                  |>.filter (· ≠ "")
                  |>.toArray
  let N := (tokens[0]!).toNat!
  let M := (tokens[1]!).toNat!
  let mut idx := 2
  let mut adj : Array (List Nat) := Array.mkArray (N+1) []
  let mut indeg : Array Nat := Array.mkArray (N+1) 0
  for _ in [0:M] do
    let t0 := (tokens[idx]!).toNat!
    let k := (tokens[idx+1]!).toNat!
    idx := idx + 2
    for _ in [0:k] do
      let dep := (tokens[idx]!).toNat!
      idx := idx + 1
      adj := adj.set! dep (t0 :: adj[dep]!)
      indeg := indeg.set! t0 (indeg[t0]! + 1)
  let mut res : Array Nat := #[]
  let mut used : Array Bool := Array.mkArray (N+1) false
  for _ in [0:N] do
    let mut v := 1
    let mut chosen := 0
    while v ≤ N do
      if !used[v]! && indeg[v]! == 0 then
        chosen := v
        v := N + 1
      else
        v := v + 1
    res := res.push chosen
    used := used.set! chosen true
    for nxt in adj[chosen]! do
      indeg := indeg.set! nxt (indeg[nxt]! - 1)
  let out := String.intercalate " " (res.toList.map toString)
  IO.println out
