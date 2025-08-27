/- Solution for SPOJ STABLEMP - Stable Marriage Problem
https://www.spoj.com/problems/STABLEMP/
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
    if c ≥ '0'.toNat && c ≤ '9'.toNat then
      num := num * 10 + (c.toNat - '0'.toNat)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then
    arr := arr.push num
  return arr

/-- Solve a single test case given tokens starting at index `idx`. -/
partial def solveCase (nums : Array Nat) (idx : Nat) : (String × Nat) := Id.run do
  let n := nums.get! idx
  let mut i := idx + 1
  -- women's preferences
  let mut women : Array (Array Nat) := Array.mkArray (n+1) #[]
  for _ in [0:n] do
    let w := nums.get! i
    i := i + 1
    let mut prefs : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      prefs := prefs.push (nums.get! i)
      i := i + 1
    women := women.set! w prefs
  -- men's preferences
  let mut men : Array (Array Nat) := Array.mkArray (n+1) #[]
  for _ in [0:n] do
    let m := nums.get! i
    i := i + 1
    let mut prefs : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      prefs := prefs.push (nums.get! i)
      i := i + 1
    men := men.set! m prefs
  -- ranking for women
  let mut rank : Array (Array Nat) := Array.mkArray (n+1) (Array.mkArray (n+1) 0)
  for w in [1:n+1] do
    let prefs := women.get! w
    let mut r := Array.mkArray (n+1) 0
    for j in [0:n] do
      let man := prefs.get! j
      r := r.set! man j
    rank := rank.set! w r
  -- Gale-Shapley
  let mut next : Array Nat := Array.mkArray (n+1) 0
  let mut manPartner : Array Nat := Array.mkArray (n+1) 0
  let mut womanPartner : Array Nat := Array.mkArray (n+1) 0
  let mut free : Std.Queue Nat := {}
  for m in [1:n+1] do
    free := free.enqueue m
  while let some (m, q') := free.dequeue? do
    free := q'
    let prefs := men.get! m
    let idx := next.get! m
    let w := prefs.get! idx
    next := next.set! m (idx + 1)
    let cur := womanPartner.get! w
    if cur == 0 then
      womanPartner := womanPartner.set! w m
      manPartner := manPartner.set! m w
    else
      let wrank := rank.get! w
      if wrank.get! m < wrank.get! cur then
        manPartner := manPartner.set! cur 0
        free := free.enqueue cur
        womanPartner := womanPartner.set! w m
        manPartner := manPartner.set! m w
      else
        free := free.enqueue m
  -- build output lines
  let mut lines : Array String := #[]
  for m in [1:n+1] do
    let w := manPartner.get! m
    lines := lines.push s!"{m} {w}"
  return (String.intercalate "\n" lines, i)

/-- Process all test cases. -/
partial def solveAll (nums : Array Nat) : String :=
  let t := nums.get! 0
  let mut idx := 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let (ans, ni) := solveCase nums idx
    outs := outs.push ans
    idx := ni
  String.intercalate "\n" outs

def main : IO Unit := do
  let nums ← readInts
  IO.println (solveAll nums)

