/- Solution for SPOJ TWORK - Teamwork is Crucial
https://www.spoj.com/problems/TWORK/
-/
import Std
open Std

/-- read all integers from stdin --/
def readInts : IO (Array Int) := do
  let stdin ← IO.getStdin
  let s ← stdin.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- return array of unused neighbors for vertex v --/
def freeNeighbors (adj : Array (Array Nat)) (used : Array Bool) (v : Nat) : Array Nat :=
  Id.run do
    let mut res : Array Nat := #[]
    for u in adj[v]! do
      if !used[u]! then
        res := res.push u
    return res

partial def build (adj : Array (Array Nat)) (n : Nat) : IO (Array (Array Nat)) := do
  let usedRef ← IO.mkRef (Array.replicate (n+1) false)
  let resRef ← IO.mkRef (Array.mkEmpty (n / 3))
  let rec loop : IO Unit := do
    let used ← usedRef.get
    let mut cand : Option Nat := none
    for i in [1:n+1] do
      if cand.isNone && !used[i]! then
        let nb := freeNeighbors adj used i
        if nb.size == 2 then
          cand := some i
    match cand with
    | some v => do
        let used ← usedRef.get
        let nb := freeNeighbors adj used v
        let a := nb[0]!
        let b := nb[1]!
        usedRef.set (used.set! v true |>.set! a true |>.set! b true)
        let res ← resRef.get
        resRef.set (res.push #[v, a, b])
        loop
    | none => do
        let used ← usedRef.get
        let mut vOpt : Option Nat := none
        for i in [1:n+1] do
          if vOpt.isNone && !used[i]! then
            vOpt := some i
        match vOpt with
        | some v => do
            let used ← usedRef.get
            let nb := freeNeighbors adj used v
            if nb.size < 2 then
              pure ()
            else
              let a := nb[0]!
              let b := nb[1]!
              usedRef.set (used.set! v true |>.set! a true |>.set! b true)
              let res ← resRef.get
              resRef.set (res.push #[v, a, b])
              loop
        | none => pure ()
  loop
  resRef.get

/-- solve single test case --/
def solveCase (n : Nat) (_m : Nat) (edges : Array (Nat × Nat)) : IO (Array (Array Nat)) := do
  let mut adj : Array (Array Nat) := Array.replicate (n+1) (#[] : Array Nat)
  for (a,b) in edges do
    adj := adj.set! a ((adj[a]!).push b)
    adj := adj.set! b ((adj[b]!).push a)
  build adj n

/-- main: read input and output groups --/
def main : IO Unit := do
  let data ← readInts
  if data.size = 0 then return
  let t := Int.toNat (data[0]!)
  let mut idx := 1
  for _ in [0:t] do
    let n := Int.toNat (data[idx]!)
    let m := Int.toNat (data[idx+1]!)
    idx := idx + 2
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let a := Int.toNat (data[idx]!)
      let b := Int.toNat (data[idx+1]!)
      idx := idx + 2
      edges := edges.push (a,b)
    let groups ← solveCase n m edges
    IO.println groups.size
    for g in groups do
      IO.println s!"{g[0]!} {g[1]!} {g[2]!}"
