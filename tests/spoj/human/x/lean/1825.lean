/- Solution for SPOJ FTOUR2 - Free tour II
https://www.spoj.com/problems/FTOUR2/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Negative infinity for DP. --/
def negInf : Int := -1000000000000000

partial def dfs (adj : Array (Array (Nat × Int))) (crowded : Array Bool)
    (k : Nat) (ansRef : IO.Ref Int) (u parent : Nat) : IO (Array Int) := do
  let base := if crowded[u]! then 1 else 0
  let mut best : Array Int := Array.replicate (k + 1) negInf
  if base ≤ k then
    best := best.set! base 0
  for (v, w) in adj[u]! do
    if v ≠ parent then
      let child ← dfs adj crowded k ansRef v u
      -- combine for global answer
      for i in [0:k+1] do
        let bi := best[i]!
        if bi != negInf then
          for j in [0:k+1] do
            if i + j ≤ k then
              let cj := child[j]!
              if cj != negInf then
                let cand := bi + cj + w
                let cur ← ansRef.get
                if cand > cur then
                  ansRef.set cand
      -- update best with path via this child
      for j in [0:k+1] do
        if base + j ≤ k then
          let cj := child[j]!
          if cj != negInf then
            let val := cj + w
            if val > best[base + j]! then
              best := best.set! (base + j) val
  return best

def main : IO Unit := do
  let data ← readInts
  let n : Nat := Int.toNat (data[0]!)
  let k : Nat := Int.toNat (data[1]!)
  let m : Nat := Int.toNat (data[2]!)
  let mut idx := 3
  let mut crowded : Array Bool := Array.replicate (n + 1) false
  for _ in [0:m] do
    let id := Int.toNat (data[idx]!)
    crowded := crowded.set! id true
    idx := idx + 1
  let mut adj : Array (Array (Nat × Int)) := Array.replicate (n + 1) #[]
  for _ in [0:n-1] do
    let a := Int.toNat (data[idx]!)
    let b := Int.toNat (data[idx+1]!)
    let w := data[idx+2]!
    adj := adj.set! a ((adj[a]!).push (b, w))
    adj := adj.set! b ((adj[b]!).push (a, w))
    idx := idx + 3
  let ansRef ← IO.mkRef (0 : Int)
  _ ← dfs adj crowded k ansRef 1 0
  let ans ← ansRef.get
  IO.println ans
