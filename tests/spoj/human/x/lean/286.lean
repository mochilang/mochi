/- Solution for SPOJ SCITIES - Selfish Cities
https://www.spoj.com/problems/SCITIES/
-/

import Std
open Std

/-- Hungarian algorithm for maximum weight matching in bipartite graphs. `a` is an
    n x m matrix of weights (0-indexed). Returns maximal sum of selected weights. -/
private def hungarian (n m : Nat) (a : Array (Array Int)) : Int :=
  let size := if n > m then n else m
  Id.run do
    -- extend matrix to 1-indexed square array
    let mut cost : Array (Array Int) := Array.mkArray (size+1) (Array.mkArray (size+1) 0)
    for i in [0:n] do
      for j in [0:m] do
        cost := cost.modify (i+1) (fun row => row.set! (j+1) ((a.get! i).get! j))
    let mut u : Array Int := Array.mkArray (size+1) 0
    let mut v : Array Int := Array.mkArray (size+1) 0
    let mut p : Array Nat := Array.mkArray (size+1) 0
    let mut way : Array Nat := Array.mkArray (size+1) 0
    for i in [1:size+1] do
      p := p.set! 0 i
      let mut j0 : Nat := 0
      let inf : Int := 1000000000
      let mut minv : Array Int := Array.mkArray (size+1) inf
      let mut used : Array Bool := Array.mkArray (size+1) false
      while true do
        used := used.set! j0 true
        let i0 := p.get! j0
        let mut delta : Int := inf
        let mut j1 : Nat := 0
        for j in [1:size+1] do
          if !(used.get! j) then
            let cur := - (cost.get! i0).get! j - u.get! i0 - v.get! j
            if cur < minv.get! j then
              minv := minv.set! j cur
              way := way.set! j j0
            if minv.get! j < delta then
              delta := minv.get! j
              j1 := j
        for j in [0:size+1] do
          if used.get! j then
            let pj := p.get! j
            u := u.set! pj (u.get! pj + delta)
            v := v.set! j (v.get! j - delta)
          else
            minv := minv.set! j (minv.get! j - delta)
        j0 := j1
        if p.get! j0 == 0 then
          break
      while j0 ≠ 0 do
        let j1 := way.get! j0
        p := p.set! j0 (p.get! j1)
        j0 := j1
    let mut res : Int := 0
    for j in [1:m+1] do
      let i := p.get! j
      if i ≤ n ∧ j ≤ m then
        res := res + (cost.get! i).get! j
    return res

/-- read all integers from stdin --/
private def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- solve the Selfish Cities problem --/
def main : IO Unit := do
  let data ← readInts
  let mut idx : Nat := 0
  let t := data.get! idx; idx := idx + 1
  let mut outputs : Array String := #[]
  for _ in [0:t.toNat] do
    let c1 := data.get! idx; idx := idx + 1
    let c2 := data.get! idx; idx := idx + 1
    let n := c1.toNat
    let m := c2.toNat
    let mut mat : Array (Array Int) := Array.mkArray n (Array.mkArray m 0)
    while true do
      let a := data.get! idx; idx := idx + 1
      let b := data.get! idx; idx := idx + 1
      let g := data.get! idx; idx := idx + 1
      if a = 0 ∧ b = 0 ∧ g = 0 then
        break
      else
        mat := mat.modify (a.toNat - 1) (fun row => row.set! (b.toNat - 1) g)
    let ans := hungarian n m mat
    outputs := outputs.push (toString ans)
  IO.println (String.intercalate "\n" outputs)
