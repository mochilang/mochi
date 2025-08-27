/- Solution for SPOJ MATH1 - Math I
https://www.spoj.com/problems/MATH1/
-/

import Std
open Std

/-- compute minimal sum for one test case using DP over median values -/
def solve (a : Array Nat) : Nat :=
  let n := a.size
  let mut pref : Array Nat := Array.mkEmpty n
  let mut acc := 0
  for x in a do
    acc := acc + x
    pref := pref.push acc
  let s := acc
  let inf := (1000000000 : Nat)
  let mut best := inf
  for m in [0:s+1] do
    let mut dp := Array.replicate (s+1) inf
    dp := dp.set! 0 0
    for i in [0:n] do
      let pv := pref[i]!
      let lim := Nat.min s i
      let mut ndp := Array.replicate (s+1) inf
      for k in [0:lim+1] do
        let diff : Int := Int.ofNat pv - Int.ofNat k - Int.ofNat m
        let cost := dp[k]! + diff.natAbs
        let old0 := ndp[k]!
        let ndp0 := if cost < old0 then cost else old0
        ndp := ndp.set! k ndp0
        if k+1 <= s then
          let old1 := ndp[k+1]!
          let ndp1 := if cost < old1 then cost else old1
          ndp := ndp.set! (k+1) ndp1
      dp := ndp
    let cand := dp[s]!
    if cand < best then best := cand
  best

partial def processCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let nums := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
    let arr := nums.map (fun s => s.toNat!) |>.toArray
    let ans := solve arr
    IO.println ans
    processCases h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  processCases h t
