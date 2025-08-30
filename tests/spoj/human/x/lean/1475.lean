/- Solution for SPOJ WORMS - Play with Digit Seven
https://www.spoj.com/problems/WORMS/
-/

import Std
open Std

/-- Count good numbers in [1,10^n]. -/
def countGood (n : Nat) : Nat := Id.run do
  let mut dp : Array Nat := Array.mkArray 7 0
  dp := dp.set! 0 1
  for _ in [0:n] do
    let mut nxt : Array Nat := Array.mkArray 7 0
    for m in [0:7] do
      let v := dp.get! m
      if v != 0 then
        for d in [0:10] do
          if d != 7 then
            let idx := (m * 10 + d) % 7
            let old := nxt.get! idx
            nxt := nxt.set! idx (old + v)
    dp := nxt
  let mut bad : Nat := 1 -- include 10^n
  for m in [1:7] do
    bad := bad + dp.get! m
  let total := Nat.pow 10 n
  return total - bad

/-- Parse all naturals from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ '0'.toNat && c ≤ '9'.toNat then
      num := num * 10 + (c.toNat - '0'.toNat)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then arr := arr.push num
  return arr

def main : IO Unit := do
  let nums ← readInts
  let t := nums.get! 0
  let mut idx := 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := nums.get! idx
    idx := idx + 1
    outs := outs.push (toString (countGood n))
  IO.println (String.intercalate "\n" outs)
