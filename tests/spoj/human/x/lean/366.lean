/- Solution for SPOJ FARMER - Farmer
https://www.spoj.com/problems/FARMER/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Solve a single test case starting at index `start` in `data`.
    Returns output string and new index. -/
partial def solveCase (data : Array Nat) (start : Nat) : (String × Nat) := Id.run do
  let q := data.get! start
  let m := data.get! (start + 1)
  let k := data.get! (start + 2)
  let mut idx := start + 3
  -- fields
  let mut fields : Array Nat := Array.mkEmpty m
  for _ in [0:m] do
    fields := fields.push (data.get! idx)
    idx := idx + 1
  -- strips
  let mut strips : Array Nat := Array.mkEmpty k
  for _ in [0:k] do
    strips := strips.push (data.get! idx)
    idx := idx + 1
  -- sort fields descending and take whole ones greedily
  fields := fields.qsort (fun a b => b < a)
  let mut rem := q
  let mut caps : Array Nat := Array.mkEmpty (m + k)
  for f in fields do
    if rem >= f then
      rem := rem - f
    else
      caps := caps.push f
  -- add strips as capacities
  for r in strips do
    caps := caps.push r
  if rem == 0 then
    return (toString q, idx)
  -- sort capacities descending and take as few segments as possible
  caps := caps.qsort (fun a b => b < a)
  let mut sum := 0
  let mut seg := 0
  for c in caps do
    if sum < rem then
      sum := sum + c
      seg := seg + 1
    else
      break
  let ans := q - seg
  (toString ans, idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (out, idx') := solveCase data idx
    IO.println out
    idx := idx'
