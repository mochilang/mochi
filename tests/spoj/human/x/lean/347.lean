/- Solution for SPOJ LAZYCOWS - Lazy Cows
https://www.spoj.com/problems/LAZYCOWS/
-/
import Std
open Std

partial def solveCase (toks : Array String) (startIdx : Nat) : (String × Nat) :=
  Id.run do
    let n := toks[startIdx]!.toNat!
    let k0 := toks[startIdx+1]!.toNat!
    let _b := toks[startIdx+2]!.toNat!
    let mut idx := startIdx + 3
    let mut colMask : Std.HashMap Nat Nat := {}
    let mut cols : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      let r := toks[idx]!.toNat!
      let c := toks[idx+1]!.toNat!
      idx := idx + 2
      match colMask.find? c with
      | some m =>
          let newMask :=
            if r == 1 then (if m % 2 == 0 then m + 1 else m)
            else (if m < 2 then m + 2 else m)
          colMask := colMask.insert c newMask
      | none =>
          let mask := if r == 1 then 1 else 2
          colMask := colMask.insert c mask
          cols := cols.push c
    let cols := cols.qsort (· < ·)
    let m := cols.size
    let k := if k0 > m then m else k0
    let mut masks : Array Nat := Array.mkEmpty m
    for c in cols do
      masks := masks.push (colMask.find! c)
    let mut pref1 : Array Nat := Array.mkArray (m+1) 0
    let mut pref2 : Array Nat := Array.mkArray (m+1) 0
    for i in [0:m] do
      let mask := masks[i]!
      pref1 := pref1.set! (i+1) (pref1[i]! + (if mask % 2 == 1 then 1 else 0))
      pref2 := pref2.set! (i+1) (pref2[i]! + (if mask >= 2 then 1 else 0))
    let inf : Nat := 1000000000000
    let mut dp : Array (Array Nat) := Array.replicate (k+1) (Array.replicate (m+1) inf)
    dp := dp.set! 0 ((dp[0]!).set! 0 0)
    for kk in [1:k+1] do
      for ii in [1:m+1] do
        let mut best := inf
        for j in [kk-1:ii] do
          let width := cols[ii-1]! - cols[j]! + 1
          let c1 := pref1[ii]! - pref1[j]!
          let c2 := pref2[ii]! - pref2[j]!
          let area := if c1 > 0 && c2 > 0 then width * 2 else width
          let cand := dp[kk-1]![j]! + area
          if cand < best then
            best := cand
        let row := dp[kk]!
        dp := dp.set! kk (row.set! ii best)
    (toString (dp[k]![m]!), idx)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "") |>.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    let mut idx := 1
    let mut outs : Array String := Array.mkEmpty t
    for _ in [0:t] do
      let (res, idx') := solveCase toks idx
      outs := outs.push res
      idx := idx'
    for s in outs do
      IO.println s
