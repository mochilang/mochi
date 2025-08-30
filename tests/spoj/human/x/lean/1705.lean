/- Solution for SPOJ AVARY - Avaricious Maryanna
https://www.spoj.com/problems/AVARY/
-/

import Std
open Std

-- compute next value in automorphic sequence
private def next (x q pow10 inv : Nat) : Nat × Nat :=
  let k := (q % 10 * inv) % 10
  let x' := x + k * pow10
  let q' := (q + k * (2 * x + k * pow10 - 1)) / 10
  (x', q')

-- precompute all solutions up to 500 digits
private def precompute (maxN : Nat) : Array (List Nat) :=
  let mut arr := Array.mkArray (maxN + 1) []
  arr := arr.set! 1 [0, 1, 5, 6]
  let mut x5 := 5
  let mut q5 := (x5 * x5 - x5) / 10
  let mut x6 := 6
  let mut q6 := (x6 * x6 - x6) / 10
  let mut pow10 := 10
  for n in [2:maxN+1] do
    let (x5', q5') := next x5 q5 pow10 1 -- inverse of (1 - 2*5) mod 10
    let (x6', q6') := next x6 q6 pow10 9 -- inverse of (1 - 2*6) mod 10
    x5 := x5'; q5 := q5'
    x6 := x6'; q6 := q6'
    let mut sols : List Nat := []
    if x5 ≥ pow10 then sols := x5 :: sols
    if x6 ≥ pow10 then sols := x6 :: sols
    arr := arr.set! n (sols.qsort (· < ·))
    pow10 := pow10 * 10
  arr

private def pre : Array (List Nat) := precompute 500

private def solve (n : Nat) : List Nat :=
  pre.get! n

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for i in [1:t.succ] do
    let n := (← h.getLine).trim.toNat!
    let sols := solve n
    if sols.isEmpty then
      IO.println s!"Case #{i}: Impossible"
    else
      let nums := sols.map toString
      IO.println s!"Case #{i}: {String.intercalate " " nums}"
