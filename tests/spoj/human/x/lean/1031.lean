/- Solution for SPOJ UPSUB - Up Subsequence
https://www.spoj.com/problems/UPSUB/
-/

import Std
open Std

private def lndsDP (arr : Array Char) : Array Nat :=
  let n := arr.size
  Id.run <| do
    let mut dp := Array.mkArray n 1
    for k in [0:n] do
      let i := n - 1 - k
      let c := arr.get! i
      let mut best := 1
      for j in [i+1:n] do
        let c2 := arr.get! j
        if c2 ≥ c then
          let cand := dp.get! j + 1
          if cand > best then
            best := cand
      dp := dp.set! i best
    pure dp

partial def firstIdx (arr : Array Char) (dp : Array Nat)
    (start : Nat) (prev : Char) (len : Nat) (c : Char) : Nat :=
  let n := arr.size
  if h : start < n then
    let ch := arr.get! start
    if ch = c ∧ prev ≤ ch ∧ dp.get! start = len then
      start
    else
      firstIdx arr dp (start+1) prev len c
  else
    n

partial def build (arr : Array Char) (dp : Array Nat) :
    Nat → Char → Nat → List String
  | start, prev, 0 => ["" ]
  | start, prev, len =>
    let n := arr.size
    let mut set : RBSet Char := {}
    for i in [start:n] do
      let c := arr.get! i
      if prev ≤ c ∧ dp.get! i = len then
        set := set.insert c
    set.toList.foldl (fun acc c =>
      let idx := firstIdx arr dp start prev len c
      let tails := build (idx+1) c (len-1)
      acc ++ tails.map (fun t => String.singleton c ++ t)
    ) []

private def solve (s : String) : List String :=
  let arr := Array.mk s.toList
  let dp := lndsDP arr
  let L := dp.foldl (fun m x => max m x) 0
  build arr dp 0 (Char.ofNat 0) L

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let xs := solve line.trim
    for x in xs do
      IO.println x
    IO.println ""
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
