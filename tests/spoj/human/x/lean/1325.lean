/- Solution for SPOJ PARTSUM - Partial Sums
https://www.spoj.com/problems/PARTSUM/
-/

import Std
open Std

partial def solveCase (n k p : Nat) (a : Array Nat) : Nat :=
  let mut pref : Array Nat := Array.mkArray (n+1) 0
  for i in [:n] do
    let prev := pref.get! i
    let v := a.get! i
    pref := pref.set! (i+1) ((prev + v) % p)
  let mut ans := p
  for i in [:n] do
    for j in [i+1:n+1] do
      let s := (pref.get! j + p - pref.get! i) % p
      if s ≥ k && s < ans then
        ans := s
  ans % p

partial def process (toks : Array String) (idx cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let n := (toks.get! idx).toNat!
    let k := (toks.get! (idx+1)).toNat!
    let p := (toks.get! (idx+2)).toNat!
    let mut i := idx + 3
    let mut arr : Array Nat := Array.mkEmpty n
    for _ in [:n] do
      arr := arr.push ((toks.get! i).toNat!)
      i := i + 1
    let ans := solveCase n k p arr
    IO.println ans
    process toks i (cases - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let t := (toks.get! 0).toNat!
  process toks 1 t
