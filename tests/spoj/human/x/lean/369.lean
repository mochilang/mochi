/- Solution for SPOJ MATH1 - Math I
https://www.spoj.com/problems/MATH1/
-/

import Std
open Std

/-- compute minimal sum for one test case using DP over candidate medians -/
def solve (a : Array Nat) : Nat :=
  let n := a.size
  -- prefix sums and total S
  let (s, pref) := Id.run do
    let mut acc := 0
    let mut pf : Array Nat := Array.mkEmpty n
    for x in a do
      acc := acc + x
      pf := pf.push acc
    pure (acc, pf)
  let inf : Nat := 1000000000
  let mut best := inf
  -- try every possible median m
  for m in [0:s+1] do
    -- dp[k] = minimal cost after choosing k subtractions so far
    let mut dp := Array.replicate (s+1) inf
    dp := dp.set! 0 0
    for i in [0:n] do
      let pv := pref[i]!
      let lim := Nat.min s i
      let mut ndp := Array.replicate (s+1) inf
      for k in [0:lim+1] do
        let diff : Int := Int.ofNat pv - Int.ofNat k - Int.ofNat m
        let cost := dp[k]! + diff.natAbs
        -- stay at k
        let old0 := ndp[k]!
        let ndp0 := if cost < old0 then cost else old0
        ndp := ndp.set! k ndp0
        -- increase to k+1 if possible
        if k+1 <= s then
          let old1 := ndp[k+1]!
          let ndp1 := if cost < old1 then cost else old1
          ndp := ndp.set! (k+1) ndp1
      dp := ndp
    let cand := dp[s]!
    if cand < best then best := cand
  best

def main : IO Unit := do
  let t := (← IO.getLine).trim.toNat!
  for _ in [0:t] do
    -- n is not needed beyond validating input length
    let _n := (← IO.getLine).trim.toNat!
    let nums := (← IO.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
    let arr := nums.map String.toNat! |>.toArray
    IO.println (solve arr)
