/- Solution for SPOJ UFAST - Unite Fast
https://www.spoj.com/problems/UFAST/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Check if all agents can move at most `m` to achieve connectivity. --/
def feasible (xs : Array Int) (d m : Int) : Bool := Id.run do
  let mut low := xs.get! 0 - m
  let mut high := xs.get! 0 + m
  for i in [1:xs.size] do
    let x := xs.get! i
    let low' := max (x - m) low
    let high' := min (x + m) (high + d)
    if low' > high' then
      return false
    low := low'
    high := high'
  return true

/-- Minimal maximal movement when agents move simultaneously. --/
def independentTime (xs : Array Int) (d : Int) : Int :=
  let rec go (lo hi : Int) :=
    if lo < hi then
      let mid := (lo + hi) / 2
      if feasible xs d mid then
        go lo mid
      else
        go (mid + 1) hi
    else
      lo
  go 0 1000

/-- Minimal total movement when agents move one at a time. --/
def sequentialTime (xs : Array Int) (d : Int) : Int :=
  let n := xs.size
  let offset := xs.get! 0
  let maxPos := xs.get! (n - 1)
  let range : Nat := Int.toNat (maxPos - offset) + 1
  let dNat : Nat := Int.toNat d
  let big : Int := 1000000000
  let mut prev : Array Int := Array.mkArray range big
  for p in [:range] do
    let pos := offset + Int.ofNat p
    prev := prev.set! p (Int.abs (xs.get! 0 - pos))
  for i in [1:n] do
    let xi := xs.get! i
    let mut next : Array Int := Array.mkArray range big
    for p in [:range] do
      let pos := offset + Int.ofNat p
      let start := if p ≥ dNat then p - dNat else 0
      let stop := p
      let mut best := big
      for q in [start:stop.succ] do
        let cost := prev.get! q + Int.abs (xi - pos)
        if cost < best then best := cost
      next := next.set! p best
    prev := next
  prev.foldl (fun a b => if a < b then a else b) big

/-- Solve one test case starting at index `start` in the data array. --/
def solveCase (data : Array Int) (start : Nat) : (String × Nat) := Id.run do
  let n := data.get! start
  let d := data.get! (start + 1)
  let mut idx := start + 2
  let mut xs : Array Int := #[]
  for _ in [0:n.toNat] do
    xs := xs.push (data.get! idx)
    idx := idx + 1
  let xs := xs.qsort (· < ·)
  let indep := independentTime xs d
  let seq := sequentialTime xs d
  return (toString indep ++ " " ++ toString seq, idx)

/-- Main program: parse input and solve test cases. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t.toNat] do
    let (res, idx') := solveCase data idx
    IO.println res
    idx := idx'
