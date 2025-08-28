/- Solution for SPOJ HKNAP - Huge Knap Sack
https://www.spoj.com/problems/HKNAP/
-/

import Std
open Std

/-- Read all natural numbers from stdin. --/
def readNats : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Solve one test case. --/
def solveCase (n s y c : Nat) (items : Array (Nat × Nat)) : Int := Id.run do
  -- find best weight/volume ratio item
  let mut bestW := (items.get! 0).1
  let mut bestV := (items.get! 0).2
  for i in [1:items.size] do
    let (w,v) := items.get! i
    if w * bestV > bestW * v then
      bestW := w
      bestV := v
  -- unbounded knapsack up to limit
  let base : Nat := 20000
  let limit := base + bestV
  let mut dp := Array.mkArray (limit + 1) 0
  for (w,v) in items do
    for j in [v:limit+1] do
      let cand := dp.get! (j - v) + w
      if cand > dp.get! j then
        dp := dp.set! j cand
  -- function to get best value for capacity cap
  let bestVal (cap : Nat) : Nat :=
    if cap ≤ limit then
      dp.get! cap
    else
      let k := (cap - base) / bestV
      let rem := cap - k * bestV
      dp.get! rem + k * bestW
  -- gains for using t sacks together
  let mut gain : Array Int := Array.mkArray (s + 1) 0
  for t in [1:s+1] do
    let w := bestVal (t * y)
    let g : Int := Int.ofNat w - Int.ofNat ((t - 1) * c)
    gain := gain.set! t g
  -- DP over number of sacks
  let mut dpS : Array Int := Array.mkArray (s + 1) 0
  for i in [1:s+1] do
    let mut best : Int := 0
    for t in [1:i+1] do
      let cand := dpS.get! (i - t) + gain.get! t
      if cand > best then
        best := cand
    dpS := dpS.set! i best
  return dpS.get! s

/-- Main program --/
def main : IO Unit := do
  let data ← readNats
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let n := data.get! idx; idx := idx + 1
    let s := data.get! idx; idx := idx + 1
    let y := data.get! idx; idx := idx + 1
    let c := data.get! idx; idx := idx + 1
    let mut items : Array (Nat × Nat) := #[]
    for _ in [0:n] do
      let w := data.get! idx; idx := idx + 1
      let v := data.get! idx; idx := idx + 1
      items := items.push (w, v)
    IO.println (solveCase n s y c items)
