/- Solution for SPOJ BOOK - Booklets
https://www.spoj.com/problems/BOOK/
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
  if inNum then
    arr := arr.push num
  return arr

/-- Solve a single test case. -/
def solveCase (s q n : Nat) (pages : Array Nat) : Nat :=
  Id.run do
    -- pair page count with original index
    let mut pairs : Array (Nat × Nat) := Array.mkEmpty n
    for i in [0:n] do
      pairs := pairs.push (pages.get! i, i)
    -- sort by pages then index
    let pairs := pairs.qsort (fun a b =>
      if a.1 == b.1 then a.2 < b.2 else a.1 < b.1)
    let lip := n / s
    let r := n % s
    let uip := lip + (if r == 0 then 0 else 1)
    -- compute start index for target school
    let mut start := 0
    for i in [0:q] do
      if i < r then
        start := start + uip
      else
        start := start + lip
    let size := if q < r then uip else lip
    -- find booklet with smallest original index in slice
    let mut bestIdx := (pairs.get! start).2
    let mut bestPage := (pairs.get! start).1
    for i in [start+1:start+size] do
      let p := pairs.get! i
      if p.2 < bestIdx then
        bestIdx := p.2
        bestPage := p.1
    return bestPage

/-- Main solve function. -/
def solve : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let t := nums.get! idx
  idx := idx + 1
  let mut out : Array String := #[]
  for _ in [0:t] do
    let s := nums.get! idx; idx := idx + 1
    let q := nums.get! idx; idx := idx + 1
    let n := nums.get! idx; idx := idx + 1
    let mut pages : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      pages := pages.push (nums.get! idx)
      idx := idx + 1
    let ans := solveCase s q n pages
    out := out.push (toString ans)
  IO.println (String.intercalate "\n" out)

def main : IO Unit := solve
