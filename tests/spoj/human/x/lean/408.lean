/- Solution for SPOJ JRIDE - Jill Rides Again
https://www.spoj.com/problems/JRIDE/
-/

import Std
open Std

partial def process (h : IO.FS.Stream) (r b : Nat) : IO Unit := do
  if r > b then
    pure ()
  else
    let s := (← h.getLine).trim.toNat!
    let mut bestSum : Int := -1
    let mut bestStart : Nat := 0
    let mut bestEnd : Nat := 0
    let mut currSum : Int := 0
    let mut currStart : Nat := 1
    for idx in [:s-1] do
      let val := (← h.getLine).trim.toInt!
      currSum := currSum + val
      let currEnd := idx + 2
      if currSum > bestSum || (currSum == bestSum && currEnd - currStart > bestEnd - bestStart) then
        bestSum := currSum
        bestStart := currStart
        bestEnd := currEnd
      if currSum < 0 then
        currSum := 0
        currStart := currEnd
    if bestSum > 0 then
      IO.println s!"The nicest part of route {r} is between stops {bestStart} and {bestEnd}"
    else
      IO.println s!"Route {r} has no nice parts"
    process h (r+1) b

def main : IO Unit := do
  let h ← IO.getStdin
  let b := (← h.getLine).trim.toNat!
  process h 1 b
