/- Solution for SPOJ QUEST5 - Nail Them
https://www.spoj.com/problems/QUEST5/
-/

import Std
open Std

/-- Parse all integers from standard input. --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Greedy algorithm: sort by right endpoint and place nails. --/
def nailsNeeded (planks : Array (Nat × Nat)) : Nat := Id.run do
  if planks.isEmpty then
    return 0
  let arr := planks.qsort (fun a b => a.snd < b.snd)
  let mut last : Nat := 0
  let mut has : Bool := false
  let mut cnt : Nat := 0
  for i in [0:arr.size] do
    let (a,b) := arr[i]!
    if !has || last < a then
      cnt := cnt + 1
      last := b
      has := true
  return cnt

/-- Entry point: process all tables and print answers. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx : Nat := 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := data.get! idx; idx := idx + 1
    let mut planks : Array (Nat × Nat) := #[]
    for _ in [0:n] do
      let a := data.get! idx
      let b := data.get! (idx + 1)
      idx := idx + 2
      planks := planks.push (a,b)
    outs := outs.push s!"{nailsNeeded planks}"
  IO.println (String.intercalate "\n" outs)
