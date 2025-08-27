/- Solution for SPOJ INUMBER - Interesting number
https://www.spoj.com/problems/INUMBER/
-/

import Std
open Std

/-- Find the smallest number divisible by `n` with digit sum `n`.
The result is returned as a string. -/
def minimalDivisible (n : Nat) : String := Id.run do
  let m := n + 1
  let size := n * m
  let mut visited : Array Bool := Array.mkArray size false
  let mut parent : Array (Option Nat) := Array.mkArray size none
  let mut digit  : Array Nat  := Array.mkArray size 0
  let mut queue : Array Nat := #[]
  let mut head  : Nat := 0
  -- initialize with single digits 1..9
  for d in [1:10] do
    if d ≤ n then
      let rem := d % n
      let sum := d
      let idx := rem * m + sum
      if !(visited.get! idx) then
        visited := visited.set! idx true
        parent  := parent.set! idx none
        digit   := digit.set! idx d
        queue   := queue.push idx
  -- BFS over (remainder, sum) states
  let mut target : Option Nat := none
  while head < queue.size && target.isNone do
    let idx := queue.get! head; head := head + 1
    let rem := idx / m
    let sum := idx % m
    if rem == 0 && sum == n then
      target := some idx
    else
      for d in [0:10] do
        let newSum := sum + d
        if newSum ≤ n then
          let newRem := (rem * 10 + d) % n
          let newIdx := newRem * m + newSum
          if !(visited.get! newIdx) then
            visited := visited.set! newIdx true
            parent  := parent.set! newIdx (some idx)
            digit   := digit.set! newIdx d
            queue   := queue.push newIdx
  match target with
  | none => ""
  | some idx =>
      let rec build (i : Nat) (acc : List Char) :=
        let d := digit.get! i
        let ch := Char.ofNat (48 + d)
        match parent.get! i with
        | none   => String.mk (ch :: acc)
        | some p => build p (ch :: acc)
      build idx []

/-- Parse all integers from stdin. -/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Main program: read test cases and output answers. -/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut i := 1
  for _ in [0:t] do
    let n := data.get! i; i := i + 1
    IO.println (minimalDivisible n)
