/- Solution for SPOJ PRO - Promotion
https://www.spoj.com/problems/PRO/
-/

import Std
open Std

/-- Read all natural numbers from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ 48 && c ≤ 57 then
      num := num * 10 + (c.toNat - 48)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then
    arr := arr.push num
  return arr

/-- Compute total prize cost. -/
def solve (nums : Array Nat) : Nat :=
  Id.run do
    let n := nums.get! 0
    let mut idx := 1
    let maxV : Nat := 1000000
    let mut freq : Array Nat := Array.mkArray (maxV + 1) 0
    let mut minv : Nat := maxV
    let mut maxv : Nat := 0
    let mut total : Nat := 0
    for _ in [0:n] do
      let k := nums.get! idx
      idx := idx + 1
      for _ in [0:k] do
        let v := nums.get! idx
        idx := idx + 1
        freq := freq.set! v (freq.get! v + 1)
        if v < minv then minv := v
        if v > maxv then maxv := v
      -- remove minimum
      while minv <= maxV && freq.get! minv == 0 do
        minv := minv + 1
      let mn := minv
      freq := freq.set! mn (freq.get! mn - 1)
      while minv <= maxV && freq.get! minv == 0 do
        minv := minv + 1
      -- remove maximum
      while maxv > 0 && freq.get! maxv == 0 do
        maxv := maxv - 1
      let mx := maxv
      freq := freq.set! mx (freq.get! mx - 1)
      while maxv > 0 && freq.get! maxv == 0 do
        maxv := maxv - 1
      total := total + (mx - mn)
    return total

def main : IO Unit := do
  let nums ← readInts
  IO.println (toString (solve nums))
