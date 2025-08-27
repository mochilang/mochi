/- Solution for SPOJ MULTIPLY - Fast Multiplication Again
https://www.spoj.com/problems/MULTIPLY
-/
import Std
open Std

/-- Read two positive integers from standard input and print their product. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let a := (← h.getLine).trim.toNat!
  let b := (← h.getLine).trim.toNat!
  IO.println (a * b)
