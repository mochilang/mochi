/- Solution for SPOJ DNALAB - DNA Laboratory
https://www.spoj.com/problems/DNALAB/
-/

import Std
open Std

/-- Remove strings that are substrings of others. -/
def prune (arr : Array String) : Array String :=
  let sorted := arr.qsort (fun a b => b.length < a.length)
  let mut res : Array String := #[]
  for s in sorted do
    let mut contained := false
    for t in res do
      if t.contains s then
        contained := true
    if !contained then
      res := res.push s
  res

/-- Compute maximum overlap where suffix of `a` matches prefix of `b`. -/
def overlap (a b : String) : Nat :=
  let maxLen := min a.length b.length
  let rec loop : Nat → Nat
  | 0 => 0
  | k+1 =>
      if b.startsWith (a.drop (a.length - (k+1))) then k+1
      else loop k
  loop maxLen

/-- Choose better string: shorter or lexicographically smaller. -/
def pick (o : Option String) (s : String) : Option String :=
  match o with
  | none => some s
  | some b =>
      if s.length < b.length || (s.length == b.length && s < b) then
        some s
      else
        some b

/-- Build shortest common superstring. -/
def shortestSuperstring (arr0 : Array String) : String := Id.run do
  let arr := prune arr0
  let n := arr.size
  if n == 0 then
    return ""
  -- overlaps
  let mut ov : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
  for i in [0:n] do
    for j in [0:n] do
      if i ≠ j then
        let o := overlap (arr.get! i) (arr.get! j)
        ov := ov.set! i ((ov.get! i).set! j o)
  let total := Nat.shiftLeft 1 n
  let mut dp : Array (Array (Option String)) := Array.mkArray total (Array.mkArray n none)
  for i in [0:n] do
    dp := dp.set! (1 <<< i) ((dp.get! (1 <<< i)).set! i (some (arr.get! i)))
  for mask in [1:total] do
    for i in [0:n] do
      if (mask &&& (1 <<< i)) ≠ 0 then
        if mask ≠ (1 <<< i) then
          let pm := mask - (1 <<< i)
          let mut best : Option String := none
          for j in [0:n] do
            if j ≠ i && (pm &&& (1 <<< j)) ≠ 0 then
              match (dp.get! pm).get! j with
              | some prev =>
                  let o := (ov.get! j).get! i
                  let cand := prev ++ (arr.get! i).drop o
                  best := pick best cand
              | none => pure ()
          dp := dp.set! mask ((dp.get! mask).set! i best)
  let full := total - 1
  let mut ans : Option String := none
  for i in [0:n] do
    match (dp.get! full).get! i with
    | some s => ans := pick ans s
    | none => pure ()
  return ans.getD ""

/-- Read all whitespace separated tokens from stdin. -/
def readTokens : IO (Array String) := do
  let data ← IO.readStdin
  pure <| data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (· ≠ "") |>.toArray

/-- Main entry: parse scenarios and solve. -/
def main : IO Unit := do
  let tokens ← readTokens
  let idxRef ← IO.mkRef 0
  let next : IO String := do
    let i ← idxRef.get
    idxRef.set (i+1)
    pure <| tokens.get! i
  let nextNat : IO Nat := do
    let s ← next
    pure <| s.toNat!
  let t ← nextNat
  for caseIdx in [0:t] do
    let n ← nextNat
    let mut arr : Array String := #[]
    for _ in [0:n] do
      arr := arr.push (← next)
    let ans := shortestSuperstring arr
    IO.println s!"Scenario #{caseIdx+1}:"
    IO.println ans
    IO.println ""
