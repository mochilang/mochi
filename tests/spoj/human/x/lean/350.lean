/- Solution for SPOJ LANDSCAP - Landscaping
https://www.spoj.com/problems/LANDSCAP/
-/

import Std
open Std

private def precomputeInc (a : Array Nat) : Array (Array Nat) := Id.run do
  let n := a.size
  let mut inc := Array.mkArray n (Array.mkArray n 0)
  let mut r : Nat := 0
  while r < n do
    let mut b := a[r]!
    let mut cost : Nat := 0
    let mut l := r
    while l > 0 do
      l := l - 1
      let al := a[l]!
      if al > b then
        cost := cost + (al - b)
      else
        b := al
      let row := inc[l]!
      inc := inc.set! l (row.set! r cost)
    r := r + 1
  return inc

private def precomputeDec (a : Array Nat) : Array (Array Nat) := Id.run do
  let n := a.size
  let mut dec := Array.mkArray n (Array.mkArray n 0)
  let mut l : Nat := 0
  while l < n do
    let mut b := a[l]!
    let mut cost : Nat := 0
    let mut r := l
    while r + 1 < n do
      r := r + 1
      let ar := a[r]!
      if ar > b then
        cost := cost + (ar - b)
      else
        b := ar
      let row := dec[l]!
      dec := dec.set! l (row.set! r cost)
    l := l + 1
  return dec

private def solveCase (a : Array Nat) (K : Nat) : Nat := Id.run do
  let n := a.size
  let inc := precomputeInc a
  let dec := precomputeDec a
  let inf : Nat := 1000000000000000
  let mut best := inf
  let mut k : Nat := 1
  while k ≤ K do
    let m := 2 * k
    let mut dp : Array (Array Nat) := Array.mkArray (m+1) (Array.mkArray n inf)
    -- first increasing segment
    let mut i : Nat := 0
    while i < n do
      let row := dp[1]!
      dp := dp.set! 1 (row.set! i (inc[0]![i]!))
      i := i + 1
    -- remaining segments
    let mut j2 : Nat := 2
    while j2 ≤ m do
      let costMat := if j2 % 2 = 0 then dec else inc
      let mut i2 : Nat := 0
      while i2 < n do
        let mut bestCost := inf
        let limit : Int := if j2 ≠ m then (Int.ofNat i2) - 1 else Int.ofNat i2
        let mut p : Int := Int.ofNat (j2 - 2)
        while p ≤ limit do
          let pNat := p.toNat
          let segCost := if pNat ≠ i2 then costMat[pNat+1]![i2]! else 0
          let c := dp[j2-1]![pNat]! + segCost
          if c < bestCost then bestCost := c
          p := p + 1
        let row2 := dp[j2]!
        dp := dp.set! j2 (row2.set! i2 bestCost)
        i2 := i2 + 1
      j2 := j2 + 1
    let val := dp[m]![n-1]!
    if val < best then best := val
    k := k + 1
  return best

partial def process (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := toks[idx]!.toNat!
    let K := toks[idx+1]!.toNat!
    let mut arr : Array Nat := Array.mkEmpty n
    let mut i := 0
    let mut id := idx + 2
    while i < n do
      arr := arr.push (toks[id]!.toNat!)
      i := i + 1
      id := id + 1
    let ans := solveCase arr K
    IO.println ans
    process toks id (t-1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    process toks 1 t
