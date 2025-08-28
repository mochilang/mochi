/- Solution for SPOJ LPERMUT - Longest Permutation
https://www.spoj.com/problems/LPERMUT/
-/

import Std
open Std

partial def longestPermutation (arr : Array Nat) : Nat :=
  let n := arr.size
  let rec outer (i best : Nat) : Nat :=
    if h : i < n then
      let rec inner (j : Nat) (seen : Array Bool) (mx mn best : Nat) : Nat :=
        if h2 : j < n then
          let v := arr.get ⟨j, h2⟩
          if seen.get! v then
            best
          else
            let seen := seen.set! v true
            let mx := if v > mx then v else mx
            let mn := if v < mn then v else mn
            let len := j - i + 1
            let best :=
              if mn == 1 ∧ mx == len ∧ len > best then len else best
            inner (j+1) seen mx mn best
        else
          best
      let seen := Array.mkArray (n+1) false
      let best := inner i i seen 0 (n+1) best
      outer (i+1) best
    else
      best
  outer 0 0

partial def main : IO Unit := do
  let stdin ← IO.getStdin
  let nLine ← stdin.getLine
  let _ := nLine -- n is not needed beyond size
  let arrLine ← stdin.getLine
  let nums := arrLine.trim.split (· == ' ').map String.toNat!
  let arr : Array Nat := nums.toArray
  IO.println (longestPermutation arr)
