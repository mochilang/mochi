/- Solution for SPOJ AGGRCOW - Aggressive cows
https://www.spoj.com/problems/AGGRCOW/
-/

import Std
open Std

-- check if we can place c cows with at least distance d
def canPlace (arr : Array Int) (c d : Int) : Bool :=
  Id.run do
    let mut count : Int := 1
    let mut last := arr[0]!
    for i in [1:arr.size] do
      let x := arr[i]!
      if x - last >= d then
        count := count + 1
        last := x
    pure (count >= c)

-- binary search maximal minimum distance
partial def maxDist (arr : Array Int) (c : Int) : Int :=
  let hi := arr[arr.size - 1]! - arr[0]!
  let rec bs (lo hi ans : Int) : Int :=
    if lo > hi then ans
    else
      let mid := (lo + hi) / 2
      if canPlace arr c mid then
        bs (mid + 1) hi mid
      else
        bs lo (mid - 1) ans
  bs 0 hi 0

partial def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let t := tLine.trim.toNat!
  for _ in [0:t] do
    let header ← h.getLine
    let parts := header.trim.split (· = ' ')
    let n := (parts[0]!).toNat!
    let c : Int := (parts[1]!).toInt!
    let mut arr : Array Int := #[]
    for _ in [0:n] do
      let line ← h.getLine
      arr := arr.push line.trim.toInt!
    let arrSorted := arr.qsort (· < ·)
    IO.println s!"{maxDist arrSorted c}"
