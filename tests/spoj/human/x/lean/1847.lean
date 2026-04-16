/- Solution for SPOJ NOCHANGE - No Change
https://www.spoj.com/problems/NOCHANGE/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.getStdin.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Main solving routine --/
def main : IO Unit := do
  let data ← readInts
  let x := data.get! 0
  let k := data.get! 1
  -- prefix sums of coin values
  let mut prefix : Array Nat := #[]
  let mut sum := 0
  for i in [0:k] do
    sum := sum + data.get! (2 + i)
    prefix := prefix.push sum
  -- unbounded knapsack over prefix sums
  let mut dp := Array.mkArray (x + 1) false
  dp := dp.set! 0 true
  for w in prefix do
    for i in [w : x + 1] do
      if dp.get! (i - w) then
        dp := dp.set! i true
  IO.println (if dp.get! x then "YES" else "NO")
