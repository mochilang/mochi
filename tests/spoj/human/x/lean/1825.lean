/- Solution for SPOJ FTOUR2 - Free tour II
https://www.spoj.com/problems/FTOUR2/
-/

import Std
open Std

/-- Read all integers (possibly negative) from stdin. -/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Depth-first search returning best path values for each count of crowded nodes. -/
partial def dfs (adj : Array (Array (Nat × Int))) (crowded : Array Bool)
    (K : Nat) (ansRef : IO.Ref Int) (u p : Nat) : IO (Array Int) := do
  let base : Nat := if crowded.get! u then 1 else 0
  let negInf : Int := -1000000000000000000
  let mut arr : Array Int := Array.mkArray (K+1) negInf
  if base ≤ K then
    arr := arr.set! base 0
  for (v, w) in adj.get! u do
    if v ≠ p then
      let child ← dfs adj crowded K ansRef v u
      -- combine paths through u using child subtree
      for i in [0:K+1] do
        let val1 := arr.get! i
        if val1 != negInf then
          for j in [0:K+1] do
            let val2 := child.get! j
            if val2 != negInf && i + j ≤ K then
              let cand := val1 + val2 + w
              let cur ← ansRef.get
              if cand > cur then
                ansRef.set cand
      -- update arr with paths going into child
      for j in [0:K+1] do
        let val2 := child.get! j
        if val2 != negInf then
          let c := j + base
          if c ≤ K then
            let existing := arr.get! c
            let value := val2 + w
            if value > existing then
              arr := arr.set! c value
  -- update answer with paths starting at u
  for i in [0:K+1] do
    let v := arr.get! i
    if v != negInf then
      let cur ← ansRef.get
      if v > cur then
        ansRef.set v
  return arr

/-- Main solving function. -/
def main : IO Unit := do
  let data ← readInts
  if data.isEmpty then
    return ()
  let mut idx := 0
  let n := data.get! idx |>.toNat!; idx := idx + 1
  let K := data.get! idx |>.toNat!; idx := idx + 1
  let m := data.get! idx |>.toNat!; idx := idx + 1
  let mut crowded : Array Bool := Array.mkArray (n+1) false
  for _ in [0:m] do
    let c := data.get! idx |>.toNat!; idx := idx + 1
    crowded := crowded.set! c true
  let mut adj : Array (Array (Nat × Int)) := Array.mkArray (n+1) #[]
  for _ in [0:n-1] do
    let a := data.get! idx |>.toNat!; idx := idx + 1
    let b := data.get! idx |>.toNat!; idx := idx + 1
    let w := data.get! idx; idx := idx + 1
    adj := adj.modify a (·.push (b, w))
    adj := adj.modify b (·.push (a, w))
  let ansRef ← IO.mkRef (0 : Int)
  _ ← dfs adj crowded K ansRef 1 0
  let ans ← ansRef.get
  IO.println ans
