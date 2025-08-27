/- Solution for SPOJ CONTPACK - How to pack containers
https://www.spoj.com/problems/CONTPACK/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Pair adjacent numbers into their sums. Unpaired element is discarded. --/
def pair : List Nat → List Nat
  | x :: y :: xs => (x + y) :: pair xs
  | _            => []

/-- Solve a single test case starting at index `start` in `data`.
    Returns output string and new index. -/
def solveCase (data : Array Nat) (start : Nat) : (String × Nat) := Id.run do
  let capacity := 1001
  let n := data.get! start
  let mut idx := start + 1
  -- boxes grouped by size
  let mut boxes : Array (List Nat) := Array.mkArray capacity []
  let mut maxSize : Nat := 0
  for _ in [0:n] do
    let s := data.get! idx; idx := idx + 1
    let v := data.get! idx; idx := idx + 1
    boxes := boxes.set! s (v :: boxes.get! s)
    if s > maxSize then maxSize := s
  -- containers
  let q := data.get! idx; idx := idx + 1
  let mut cont : Array Nat := Array.mkArray capacity 0
  for _ in [0:q] do
    let s := data.get! idx; idx := idx + 1
    let c := data.get! idx; idx := idx + 1
    cont := cont.set! s (cont.get! s + c)
    if s > maxSize then maxSize := s
  -- recursive processing over sizes
  let rec process (i : Nat) (current : List Nat) (cost : Nat) : Option Nat :=
    if i > maxSize then
      some cost
    else
      let newList := (boxes.get! i ++ current).qsort (· < ·)
      let need := cont.get! i
      if newList.length < need then
        none
      else
        let used := newList.take need
        let remaining := newList.drop need
        let cost' := cost + used.foldl (· + ·) 0
        let next := pair remaining
        process (i+1) next cost'
  match process 0 [] 0 with
  | some v => (toString v, idx)
  | none   => ("No", idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (res, idx') := solveCase data idx
    IO.println res
    idx := idx'
