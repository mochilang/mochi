/- Solution for SPOJ NAPTIME - Naptime
https://www.spoj.com/problems/NAPTIME/
-/

import Std
open Std

/-- dynamic programming for a single test case.
    `startInBed` indicates whether we begin already in bed (period N selected).
-/
def maxSleep (n b : Nat) (u : Array Int) (startInBed : Bool) : Int :=
  let negInf : Int := -1000000000 -- sentinel smaller than any achievable value
  let size := b + 1
  let init0 := Array.replicate size negInf
  let init1 := Array.replicate size negInf
  let dp0 := if startInBed then init0 else init0.set! 0 0
  let dp1 := if startInBed then init1.set! 0 0 else init1
  let mut dp0 := dp0
  let mut dp1 := dp1
  for i in [0:n] do
    let ui := u[i]!
    let mut ndp0 := Array.replicate size negInf
    let mut ndp1 := Array.replicate size negInf
    for j in [0:size] do
      let v0 := dp0[j]!
      if v0 > negInf then
        ndp0 := ndp0.set! j (max (ndp0[j]!) v0) -- stay out of bed
        if j < b then
          ndp1 := ndp1.set! (j+1) (max (ndp1[j+1]!) v0) -- start new block
      let v1 := dp1[j]!
      if v1 > negInf then
        ndp0 := ndp0.set! j (max (ndp0[j]!) v1) -- get out of bed
        if j < b then
          ndp1 := ndp1.set! (j+1) (max (ndp1[j+1]!) (v1 + ui)) -- continue sleeping
    dp0 := ndp0
    dp1 := ndp1
  return max (dp0[b]!) (dp1[b]!)

/-- solve a single test case given N, B and utilities array -/
def solveCase (n b : Nat) (u : Array Int) : Int :=
  Int.max (maxSleep n b u false) (maxSleep n b u true)

/-- read test cases and output answers -/
partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then return
  let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
  let n := parts[0]! |>.toNat!
  let b := parts[1]! |>.toNat!
  let mut arr : Array Int := #[]
  for _ in [0:n] do
    arr := arr.push ((← h.getLine).trim.toInt!)
  IO.println (solveCase n b arr)
  loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
