/- Solution for SPOJ TRANK - Team Rankings
https://www.spoj.com/problems/TRANK/
-/

import Std
open Std

-- generate all permutations of a list

partial def insertAll (x : α) : List α → List (List α)
| []      => [[x]]
| y :: ys => (x :: y :: ys) :: (insertAll x ys).map (fun zs => y :: zs)

partial def perms : List α → List (List α)
| []      => [[]]
| x :: xs => (perms xs).bind (insertAll x)

def allRanks : List String :=
  (perms ['A', 'B', 'C', 'D', 'E']).map String.mk

-- convert ranking string to array of positions

def rankPos (s : String) : Array Nat := Id.run do
  let chars := Array.ofList s.data
  let mut pos := Array.mkArray 5 0
  for i in [0:5] do
    let c := chars.get! i
    pos := pos.set! (c.toNat - 'A'.toNat) i
  return pos

-- distance between two rankings

def dist (a b : Array Nat) : Nat := Id.run do
  let mut d := 0
  for i in [0:5] do
    for j in [i+1:5] do
      let cond1 := a.get! i < a.get! j
      let cond2 := b.get! i < b.get! j
      if cond1 ≠ cond2 then
        d := d + 1
  return d

-- find median ranking and value

def median (votes : List String) : String × Nat :=
  let voteArrs := votes.map rankPos
  Id.run do
    let mut bestStr := ""
    let mut bestVal := 0
    let mut first := true
    for cand in allRanks do
      let cArr := rankPos cand
      let mut sum := 0
      for v in voteArrs do
        sum := sum + dist cArr v
      if first || sum < bestVal || (sum = bestVal && cand < bestStr) then
        bestStr := cand
        bestVal := sum
        first := false
    return (bestStr, bestVal)

partial def solve (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  let n := toks.get! idx |>.toNat!
  if n = 0 then
    acc.reverse
  else
    let rec gather (i remaining : Nat) (a : List String) : (List String × Nat) :=
      if remaining = 0 then (a.reverse, i)
      else
        let s := toks.get! i
        gather (i+1) (remaining-1) (s :: a)
    let (votes, idx') := gather (idx+1) n []
    let (r, v) := median votes
    solve toks idx' ((r ++ " is the median ranking with value " ++ toString v ++ ".") :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let outs := solve toks 0 []
  for line in outs do
    IO.println line
