/- Solution for SPOJ PT07B - The Easiest Problem
https://www.spoj.com/problems/PT07B/
-/

import Std
open Std

partial def dfs (adj : Array (Array Nat)) (selRef : IO.Ref (Array (Array Nat))) (u parent : Nat) (ph : Bool) : IO Nat := do
  let neighbors := adj.get! u
  let mut gains : Array (Nat × Nat) := #[]
  let mut childCount := 0
  for v in neighbors do
    if v != parent then
      childCount := childCount + 1
      let childPh := if parent == 0 && neighbors.size == 1 then false else true
      let sv ← dfs adj selRef v u childPh
      if sv > 1 then
        gains := gains.push (sv - 1, v)
  let mut size := 1 + childCount
  let limit := if ph then 1 else 2
  let mut best1 : (Nat × Nat) := (0,0)
  let mut best2 : (Nat × Nat) := (0,0)
  for (g,v) in gains do
    if g > best1.fst then
      best2 := best1
      best1 := (g,v)
    else if g > best2.fst then
      best2 := (g,v)
  let mut selected : Array Nat := #[]
  if best1.fst > 0 then
    size := size + best1.fst
    selected := selected.push best1.snd
  if limit > 1 && best2.fst > 0 then
    size := size + best2.fst
    selected := selected.push best2.snd
  let mut sel ← selRef.get
  sel := sel.set! u selected
  selRef.set sel
  return size

partial def build (adj : Array (Array Nat)) (sel : Array (Array Nat)) (u parent : Nat) (edgesRef : IO.Ref (Array (Nat × Nat))) : IO Unit := do
  for v in adj.get! u do
    if v != parent then
      let e := if u < v then (u,v) else (v,u)
      edgesRef.modify (·.push e)
      if (sel.get! u).contains v then
        build adj sel v u edgesRef

partial def main' (toks : Array String) (idx : Nat) : IO Unit := do
  let n := (toks.get! idx).toNat!
  if n == 0 then
    return ()
  let mut adj : Array (Array Nat) := Array.mkArray (n+1) #[]
  let mut j := idx + 1
  for _ in [0:n-1] do
    let u := (toks.get! j).toNat!
    let v := (toks.get! (j+1)).toNat!
    j := j + 2
    adj := adj.modify u (·.push v)
    adj := adj.modify v (·.push u)
  let root :=
    match (List.range (n+1)).drop 1 |>.find? (fun i => (adj.get! i).size > 1) with
    | some r => r
    | none => 1
  let selRef ← IO.mkRef (Array.mkArray (n+1) #[])
  let _ ← dfs adj selRef root 0 false
  let sel ← selRef.get
  let edgesRef ← IO.mkRef (#[] : Array (Nat × Nat))
  build adj sel root 0 edgesRef
  let edges ← edgesRef.get
  IO.println (toString (edges.size + 1))
  for (a,b) in edges do
    IO.println s!"{a} {b}"

  return ()

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "") |>.toArray
  main' toks 0
