/- Solution for SPOJ MKBUDGET - Making A Budget
https://www.spoj.com/problems/MKBUDGET/
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
    if c >= 48 && c <= 57 then
      num := num * 10 + (c.toNat - 48)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then arr := arr.push num
  return arr

/-- Compute minimal cost for a given sequence of worker requirements. -/
def minCost (reqs : Array Nat) (hire salary fire : Nat) : Nat :=
  let n := reqs.size
  let maxEmp := reqs.foldl (fun m x => if x > m then x else m) 0
  let inf := (1000000000000000 : Nat)
  Id.run do
    let mut prev := Array.mkArray (maxEmp + 1) inf
    prev := prev.set! 0 0
    for i in [0:n] do
      let req := reqs.get! i
      let mut curr := Array.mkArray (maxEmp + 1) inf
      for w in [0:maxEmp+1] do
        if w >= req then
          let mut best := inf
          for p in [0:maxEmp+1] do
            let adjust := if w >= p then hire * (w - p) else fire * (p - w)
            let total := prev.get! p + adjust + salary * w
            if total < best then
              best := total
          curr := curr.set! w best
      prev := curr
    let mut ans := inf
    for w in [0:maxEmp+1] do
      let c := prev.get! w
      if c < ans then ans := c
    return ans

/-- Process all cases and print costs. -/
partial def loopCases (nums : Array Nat) (idx case : Nat) : IO Unit := do
  if h : idx < nums.size then
    let n := nums.get! idx
    if n == 0 then
      pure ()
    else
      let hire := nums.get! (idx+1)
      let salary := nums.get! (idx+2)
      let fire := nums.get! (idx+3)
      let mut reqs : Array Nat := #[]
      for j in [0:n] do
        reqs := reqs.push (nums.get! (idx+4 + j))
      let cost := minCost reqs hire salary fire
      IO.println s!"Case {case}, cost = ${cost}"
      loopCases nums (idx + 4 + n) (case + 1)
  else
    pure ()

/-- Entry point --/
def main : IO Unit := do
  let nums ← readInts
  loopCases nums 0 1
