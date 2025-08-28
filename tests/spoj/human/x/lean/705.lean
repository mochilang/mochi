/- Solution for SPOJ SUBST1 - New Distinct Substrings
https://www.spoj.com/problems/SUBST1/
-/
import Std
open Std

/-- Count number of distinct substrings of a string by
    enumerating all substrings and inserting them into a hash set. -/
def countDistinct (s : String) : Nat :=
  let n := s.length
  let mut set : Std.HashSet String := {}
  for i in [0:n] do
    for j in [i+1:n+1] do
      set := set.insert (s.extract i j)
  set.size

/-- Process multiple test cases from an array of tokens. -/
partial def solve (toks : Array String) (idx t : Nat)
    (acc : Array String) : Array String :=
  if t = 0 then acc
  else
    let s := toks.get! idx
    let ans := toString (countDistinct s)
    solve toks (idx+1) (t-1) (acc.push ans)

/-- Main entry point: read all tokens and output results. -/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
  let arr := Array.ofList toks
  let t := (arr.get! 0).toNat!
  let outs := solve arr 1 t #[]
  for line in outs do
    IO.println line
