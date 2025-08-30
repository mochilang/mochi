/- Solution for SPOJ HALLOW - Halloween treats
https://www.spoj.com/problems/HALLOW/
-/
import Std
open Std

partial def findSubset (c : Nat) (a : Array Nat) : Option (List Nat) :=
  let rec loop (i pref : Nat) (first : Array (Option Nat)) :=
    if h : i < a.size then
      let pref := (pref + a.get! i) % c
      match first.get! pref with
      | some j =>
          let len := i + 1 - j
          let inds := (List.range len).map (fun k => j + k + 1)
          some inds
      | none =>
          loop (i+1) pref (first.set! pref (some (i+1)))
    else
      none
  loop 0 0 (Array.mkArray c none |>.set! 0 (some 0))

partial def solve (nums : Array Nat) (idx : Nat) (acc : Array String) : Array String :=
  if h : idx + 1 < nums.size then
    let c := nums.get! idx
    let n := nums.get! (idx + 1)
    if c = 0 && n = 0 then
      acc
    else
      let arr := nums.extract (idx + 2) (idx + 2 + n)
      let out := match findSubset c arr with
        | some inds => String.intercalate " " (inds.map toString)
        | none => "no sweets"
      solve nums (idx + 2 + n) (acc.push out)
  else acc

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let nums := (toks.map String.toNat!).toArray
  let outs := solve nums 0 #[]
  for line in outs do
    IO.println line
