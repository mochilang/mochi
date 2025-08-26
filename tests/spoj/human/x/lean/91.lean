/- Solution for SPOJ TWOSQRS - Two squares or not two squares
https://www.spoj.com/problems/TWOSQRS/
-/

import Std
open Std

/-- Decide whether `n` is expressible as a sum of two squares. -/
private def canTwoSquares (n : Nat) : Bool :=
  if n == 0 then true
  else
    Id.run do
      let mut m := n
      let mut p := 2
      while p * p <= m do
        if m % p == 0 then
          let mut cnt := 0
          while m % p == 0 do
            m := m / p
            cnt := cnt + 1
          if p % 4 == 3 && cnt % 2 == 1 then
            return false
        p := p + 1
      if m % 4 == 3 then
        return false
      else
        return true

/-- Main procedure: read input and output results. -/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let t := (← stdin.getLine).trim.toNat!
  for _ in [0:t] do
    let n := (← stdin.getLine).trim.toNat!
    IO.println <| if canTwoSquares n then "Yes" else "No"

