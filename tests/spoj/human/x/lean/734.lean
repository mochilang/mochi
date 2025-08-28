/- Solution for SPOJ IVAN - Ivan and his interesting game
https://www.spoj.com/problems/IVAN/
-/

import Std
open Std

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let tokensList := (data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')).filter (fun s => s ≠ "")
  let tokens := tokensList.toArray
  if tokens.size < 2 then
    return
  let l1 := (tokens[0]!).toNat!
  let l2 := (tokens[1]!).toNat!
  let mut idx := 2
  -- read first sequence and subtract 1
  let mut arrA : Array Int := Array.mkEmpty l1
  for _ in [0:l1] do
    let v := (tokens[idx]!).toNat!
    arrA := arrA.push (Int.ofNat v - 1)
    idx := idx + 1
  -- read second sequence
  let mut arrB : Array Int := Array.mkEmpty l2
  for _ in [0:l2] do
    let v := (tokens[idx]!).toNat!
    arrB := arrB.push (Int.ofNat v - 1)
    idx := idx + 1
  -- reverse arrays to process from start
  let a := arrA.toList.reverse.toArray
  let b := arrB.toList.reverse.toArray
  -- prefix sums
  let mut sa : Array Int := Array.replicate (l1 + 1) 0
  for i in [0:l1] do
    sa := sa.set! (i+1) (sa[i]! + a[i]!)
  let mut sb : Array Int := Array.replicate (l2 + 1) 0
  for j in [0:l2] do
    sb := sb.set! (j+1) (sb[j]! + b[j]!)
  -- dp
  let inf : Int := 1000000000000000000
  let mut dp : Array (Array Int) := Array.replicate (l1 + 1) (Array.replicate (l2 + 1) inf)
  let row0 := (dp[0]!).set! 0 0
  dp := dp.set! 0 row0
  for i in [1:l1+1] do
    for j in [1:l2+1] do
      let mut best := inf
      for k in [0:i] do
        for l in [0:j] do
          let prev := (dp[k]!)[l]!
          let cost := prev + (sa[i]! - sa[k]!) * (sb[j]! - sb[l]!)
          if cost < best then
            best := cost
      let row := dp[i]!
      dp := dp.set! i (row.set! j best)
  let ans := (dp[l1]!)[l2]!
  IO.println (toString ans)
