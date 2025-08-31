/- Solution for SPOJ SEQPAR2 - Sequence Partition
https://www.spoj.com/problems/SEQPAR2/
-/

import Std
open Std

/-- read all integers from stdin --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- check feasibility for maximum segment sum `T` --/
def feasible (A B suf : Array Int) (limit : Int) (n : Nat) (T : Int) : Bool :=
  Id.run do
    let mut i : Nat := 0
    let mut cost : Int := 0
    while i < n do
      let mut sumB : Int := 0
      let mut maxA : Int := 0
      let mut minB : Int := Int.ofNat (1 <<< 60)
      let mut lastValid : Int := -1
      let mut maxAValid : Int := 0
      let mut r : Nat := i
      while r < n && sumB + B.get! r <= T do
        sumB := sumB + B.get! r
        let a := A.get! r
        if a > maxA then
          maxA := a
        let b := B.get! r
        if b < minB then
          minB := b
        if minB > suf.get! (r + 1) then
          lastValid := Int.ofNat r
          maxAValid := maxA
        r := r + 1
      if lastValid < 0 then
        return false
      cost := cost + maxAValid
      if cost > limit then
        return false
      i := (Int.toNat lastValid) + 1
    return true

/-- main entry --/
def main : IO Unit := do
  let data ← readInts
  let mut idx : Nat := 0
  let next : IO Int := do
    let v := data.get! idx
    idx := idx + 1
    return v
  let n := Int.toNat (← next)
  let limit := (← next)
  let mut A : Array Int := #[]
  let mut B : Array Int := #[]
  for _ in [0:n] do
    let a ← next
    let b ← next
    A := A.push a
    B := B.push b
  -- suffix maxima of A
  let mut suf : Array Int := Array.mkArray (n + 1) 0
  let mut cur : Int := 0
  for i in [0:n] do
    let idx := n - 1 - i
    let a := A.get! idx
    if a > cur then
      cur := a
    suf := suf.set! idx cur
  -- binary search on answer
  let mut lo : Int := 0
  let mut hi : Int := 0
  for b in B do
    if b > lo then
      lo := b
    hi := hi + b
  let mut l := lo
  let mut h := hi
  while l < h do
    let mid := (l + h) / 2
    if feasible A B suf limit n mid then
      h := mid
    else
      l := mid + 1
  IO.println l
