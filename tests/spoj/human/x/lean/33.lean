/- Solution for SPOJ TRIP - Trip
https://www.spoj.com/problems/TRIP
-/

import Std
open Std

-- compute all longest common subsequences of two strings
partial def lcsAll (s1 s2 : String) : List String :=
  let a := s1.toList.toArray
  let b := s2.toList.toArray
  let n := a.size
  let m := b.size
  -- DP table for LCS lengths
  let mut dp : Array (Array Nat) := Array.mkArray (n+1) (Array.mkArray (m+1) 0)
  for i in [0:n] do
    dp := dp.set! i (Array.mkArray (m+1) 0)
  for i in [1:n+1] do
    let ai := a.get! (i-1)
    for j in [1:m+1] do
      let bj := b.get! (j-1)
      let v := if ai = bj then
        (dp.get! (i-1)).get! (j-1) + 1
      else
        Nat.max ((dp.get! (i-1)).get! j) ((dp.get! i).get! (j-1))
      let row := (dp.get! i).set! j v
      dp := dp.set! i row
  -- memoized recursion to build all sequences
  let rec build (i j : Nat)
    : StateM (Std.HashMap (Nat × Nat) (List String)) (List String) := do
      let memo ← get
      match memo.find? (i, j) with
      | some res => pure res
      | none =>
        let res ←
          if i = n || j = m then
            pure [""]
          else if a.get! i = b.get! j then
            let tails ← build (i+1) (j+1)
            pure (tails.map (fun s => String.singleton (a.get! i) ++ s))
          else
            let len1 := (dp.get! (i+1)).get! j
            let len2 := (dp.get! i).get! (j+1)
            let mut acc : List String := []
            if len1 ≥ len2 then
              let l1 ← build (i+1) j
              acc := acc ++ l1
            if len2 ≥ len1 then
              let l2 ← build i (j+1)
              acc := acc ++ l2
            pure acc.eraseDups
        modify (fun m => m.insert (i, j) res)
        pure res
  let (ans, _) := (build 0 0).run Std.HashMap.empty
  ans.eraseDups.qsort (fun a b => a < b)

partial def solveCases (h : IO.FS.Stream) (t : Nat) (first : Bool) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let s1 ← h.getLine
    let s2 ← h.getLine
    let res := lcsAll s1.trim s2.trim
    if !first then IO.println ""
    for s in res do
      IO.println s
    solveCases h (t-1) false

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solveCases h t true
