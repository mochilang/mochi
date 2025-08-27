/- Solution for SPOJ MIXTURES - Mixtures
https://www.spoj.com/problems/MIXTURES/
-/

import Std
open Std

/-- Parse all natural numbers from stdin. -/
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
  if inNum then arr := arr.push num
  return arr

/-- Compute minimal smoke to mix all colors. -/
def minSmoke (a : Array Nat) : Nat :=
  Id.run do
    let n := a.size
    if n == 0 then
      return 0
    let mut pref : Array Nat := Array.mkArray (n+1) 0
    for i in [0:n] do
      pref := pref.set! (i+1) ((pref.get! i + a.get! i) % 100)
    let mut dp : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    for len in [2:n+1] do
      for i in [0:n+1-len] do
        let j := i + len - 1
        let mut best := (1000000000 : Nat)
        for k in [i:j] do
          let left := (pref.get! (k+1) + 100 - pref.get! i) % 100
          let right := (pref.get! (j+1) + 100 - pref.get! (k+1)) % 100
          let smoke := ((dp.get! i).get! k) + ((dp.get! (k+1)).get! j) + left * right
          if smoke < best then
            best := smoke
        let row := (dp.get! i).set! j best
        dp := dp.set! i row
    return ((dp.get! 0).get! (n-1))

/-- Solve by reading all test cases until EOF. -/
def solve : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let mut outLines : Array String := #[]
  while idx < nums.size do
    let n := nums.get! idx
    idx := idx + 1
    let mut colors : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      colors := colors.push (nums.get! idx)
      idx := idx + 1
    outLines := outLines.push (toString (minSmoke colors))
  IO.println (String.intercalate "\n" outLines)

def main : IO Unit := solve
