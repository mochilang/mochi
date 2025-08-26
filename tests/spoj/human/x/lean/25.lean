/- Solution for SPOJ POUR1 - Pouring water
https://www.spoj.com/problems/POUR1/
-/

import Std
open Std

partial def pour (fromCap toCap target : Nat) : Nat :=
  let rec loop (from to step : Nat) : Nat :=
    if from = target ∨ to = target then
      step
    else
      let transfer := min from (toCap - to)
      let from := from - transfer
      let to := to + transfer
      let step := step + 1
      if from = target ∨ to = target then
        step
      else if from = 0 then
        loop fromCap to (step + 1)
      else if to = toCap then
        loop from 0 (step + 1)
      else
        loop from to step
  loop fromCap 0 1

def solveCase (a b c : Nat) : Int :=
  if c > max a b ∨ c % Nat.gcd a b ≠ 0 then
    -1
  else
    let s1 := pour a b c
    let s2 := pour b a c
    Int.ofNat (min s1 s2)

partial def loopCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let a := (← h.getLine).trim.toNat!
    let b := (← h.getLine).trim.toNat!
    let c := (← h.getLine).trim.toNat!
    IO.println <| solveCase a b c
    loopCases h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loopCases h t
