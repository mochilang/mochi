/- Solution for SPOJ ZZPERM - Zig-Zag Permutation
https://www.spoj.com/problems/ZZPERM/
-/

import Std
open Std

-- compute counts of letters 'a'..'j' in the given word
 def letterCounts (w : String) : Array Nat := Id.run do
  let mut arr : Array Nat := Array.mkArray 10 0
  for c in w.data do
    let idx := c.toNat - 'a'.toNat
    arr := arr.set! idx (arr[idx]! + 1)
  return arr

-- backtracking to enumerate zig-zag permutations in lexicographic order
partial def dfs (n : Nat) (D : Nat) (counts : Array Nat) (pos : Nat)
  (prev : Option Nat) (prev2 : Option Nat) (pref : List Char) (rank : Nat) : (Nat × List String) :=
  if pos == n then
    let rank1 := rank + 1
    if rank1 % D == 0 then
      let s := String.mk pref.reverse
      (rank1, [s])
    else
      (rank1, [])
  else
    let rec loop (i : Nat) (r : Nat) (outs : List String) : (Nat × List String) :=
      if h : i < 10 then
        let cnt := counts[i]!
        let allowed :=
          if cnt = 0 then false
          else match prev, prev2 with
          | none, _ => true
          | some p, none => i ≠ p
          | some p, some pp =>
              if pp < p then p > i
              else if pp > p then p < i
              else false
        if allowed then
          let counts' := counts.set! i (cnt - 1)
          let c := Char.ofNat (i + 97)
          let (r', out') := dfs n D counts' (pos + 1) (some i) prev (c :: pref) r
          loop (i + 1) r' (outs ++ out')
        else
          loop (i + 1) r outs
      else
        (r, outs)
    loop 0 rank []

-- solve single case
 def solveCase (w : String) (D : Nat) : (List String × Nat) :=
  let counts := letterCounts w
  let n := w.length
  dfs n D counts 0 none none [] 0

-- read input tokens
 def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array String := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p
  return arr

 def main : IO Unit := do
  let tokens ← readTokens
  let nCases := tokens.size / 2
  for idx in [0:nCases] do
    let w := tokens[2*idx]!
    let d := tokens[2*idx + 1]!.toNat!
    let (outs, total) := solveCase w d
    for o in outs do
      IO.println o
    IO.println (toString total)
    IO.println ""
