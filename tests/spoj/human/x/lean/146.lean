/- Solution for SPOJ MULTIPLY - Fast Multiplication Again
https://www.spoj.com/problems/MULTIPLY/
-/

import Std
open Std

-- Reads two natural numbers from stdin and prints their product.
def main : IO Unit := do
  let stdin ← IO.getStdin
  let aLine ← stdin.getLine
  let bLine ← stdin.getLine
  let a := aLine.trim.toNat!
  let b := bLine.trim.toNat!
  IO.println (a * b)

