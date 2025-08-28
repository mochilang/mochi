/- Solution for SPOJ KMSL4B - Roots of polynomial
https://www.spoj.com/problems/KMSL4B/
-/

import Std
open Std

/-- split input by whitespace into array --/
def readTokens : IO (Array String) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (·.length > 0)

/-- check if all roots of polynomial lie in open unit disc using Schur-Cohn test --/
partial def stable (coeffs : Array Float) : Bool :=
  let rec loop (arr : Array Float) : Bool :=
    let n := arr.size
    if n ≤ 1 then
      true
    else
      let a0 := arr[0]!
      let an := arr[n-1]!
      if Float.abs a0 ≥ Float.abs an then
        false
      else
        let mut newArr : Array Float := Array.mkEmpty (n-1)
        for i in [0:n-1] do
          let b := an * arr[i+1]! - a0 * arr[n-2 - i]!
          newArr := newArr.push b
        loop newArr
  loop coeffs

/-- main program: parse input and output 1 or 0 for each polynomial --/
def main : IO Unit := do
  let toks ← readTokens
  if toks.size = 0 then
    return
  let t := toks[0]!.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let deg := toks[idx]!.toNat!
    idx := idx + 1
    let mut coeffs : Array Float := Array.mkEmpty (deg+1)
    for _ in [0:deg+1] do
      coeffs := coeffs.push (toks[idx]!.toFloat!)
      idx := idx + 1
    if stable coeffs then
      IO.println "1"
    else
      IO.println "0"
