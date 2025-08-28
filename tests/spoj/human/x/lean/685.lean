/- Solution for SPOJ SEQPAR - Partition the sequence
https://www.spoj.com/problems/SEQPAR/
-/

import Std
open Std

private def feasible (arr : Array Int) (k : Nat) (m : Int) : Bool :=
  Id.run <| do
    let mut segs := 1
    let mut sum : Int := 0
    for x in arr do
      if x > m then
        return false
      if sum + x > m then
        segs := segs + 1
        sum := x
      else
        sum := sum + x
    return segs <= k

private def minimalM (arr : Array Int) (k : Nat) : Int :=
  let total := arr.foldl (fun s x => s + x) 0
  let maxElem := arr.foldl (fun m x => max m x) (arr.get! 0)
  let lo := maxElem
  let hi := if total > maxElem then total else maxElem
  let rec search (lo hi : Int) : Int :=
    if lo >= hi then lo else
      let mid := lo + (hi - lo) / 2
      if feasible arr k mid then search lo mid else search (mid + 1) hi
  search lo hi

partial def solveTokens (toks : Array String) (idx cases : Nat) (acc : List String) : List String :=
  if cases = 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let k := toks.get! (idx+1) |>.toNat!
    let mut arr : Array Int := Array.mkArray n 0
    let mut i := idx + 2
    for j in [0:n] do
      arr := arr.set! j (toks.get! i |>.toInt!)
      i := i + 1
    let ans := minimalM arr k
    solveTokens toks i (cases - 1) (toString ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
             |>.filter (fun s => s ≠ "") |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let lines := solveTokens toks 1 t []
  for line in lines do
    IO.println line
