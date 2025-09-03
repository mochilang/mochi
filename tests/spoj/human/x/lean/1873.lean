/- Solution for SPOJ ACARGO - Accumulate Cargo
https://www.spoj.com/problems/ACARGO/
-/
import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Compute minimal effort to gather cargo on circular belt. --/
def solve (L : Int) (pos : Array Int) : Int := Id.run do
  let n := pos.size
  let sorted := pos.qsort (· < ·)
  let n2 := n * 2
  -- extended positions
  let mut p2 := Array.mkArray n2 (0 : Int)
  for i in [0:n] do
    let v := sorted[i]!
    p2 := p2.set! i v
    p2 := p2.set! (i + n) (v + L)
  -- b[i] = p2[i] - i
  let mut b := Array.mkArray n2 (0 : Int)
  for i in [0:n2] do
    b := b.set! i (p2[i]! - Int.ofNat i)
  -- prefix sums of b
  let mut pref := Array.mkArray (n2 + 1) (0 : Int)
  for i in [0:n2] do
    pref := pref.set! (i + 1) (pref[i]! + b[i]!)
  -- sliding window
  let mut ans : Int := Int.ofNat (1 <<< 62)
  for s in [0:n] do
    let l := s
    let r := s + n - 1
    let m := l + n / 2
    let med := b[m]!
    let left := med * Int.ofNat (m - l) - (pref[m]! - pref[l]!)
    let right := (pref[r + 1]! - pref[m + 1]!) - med * Int.ofNat (r - m)
    let cost := left + right
    if cost < ans then
      ans := cost
  return ans

/-- Main program: parse input and process test cases. --/
def main : IO Unit := do
  let data ← readInts
  let mut idx : Nat := 0
  while idx + 1 < data.size do
    let n := data[idx]!
    let L := data[idx + 1]!
    idx := idx + 2
    if n == 0 && L == 0 then
      break
    let mut pos : Array Int := #[]
    for _ in [0:n] do
      pos := pos.push (Int.ofNat (data[idx]!))
      idx := idx + 1
    let ans := solve (Int.ofNat L) pos
    IO.println ans

