/- Solution for SPOJ ASSIGN - Assignments
https://www.spoj.com/problems/ASSIGN/
-/

import Std
open Std

abbrev Matrix := Array (Array Bool)
abbrev Memo := Std.HashMap Nat Nat

partial def dfs (i n : Nat) (mask : Nat) (pref : Matrix) : StateM Memo Nat := do
  let memo ← get
  if let some v := memo.find? mask then
    return v
  if i = n then
    modify (fun m => m.insert mask 1)
    return 1
  let row := pref.get! i
  let mut total := 0
  for j in [0:n] do
    if row.get! j && !Nat.testBit mask j then
      let val ← dfs (i+1) n (mask + Nat.pow 2 j) pref
      total := total + val
  modify (fun m => m.insert mask total)
  return total

def countAssignments (pref : Matrix) : Nat :=
  let n := pref.size
  (dfs 0 n 0 pref).run' {}

def readTokens : IO (Array String) := do
  let s ← IO.readStdin
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (fun x => x ≠ "")

def main : IO Unit := do
  let tokens ← readTokens
  let t := (tokens.get! 0).toNat!
  let mut idx := 1
  for _ in [0:t] do
    let n := (tokens.get! idx).toNat!; idx := idx + 1
    let mut pref : Matrix := Array.mkArray n (Array.mkArray n false)
    for i in [0:n] do
      let mut row := pref.get! i
      for j in [0:n] do
        let v := (tokens.get! idx).toNat!; idx := idx + 1
        row := row.set! j (v = 1)
      pref := pref.set! i row
    let ans := countAssignments pref
    IO.println (toString ans)
