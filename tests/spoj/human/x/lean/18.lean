/- Solution for SPOJ CRYPTO2 - The Bytelandian Cryptographer (Act II)
https://www.spoj.com/problems/CRYPTO2/
-/

import Std
open Std

def main : IO Unit := do
  let line ← IO.getStdin.getLine
  let n := line.trim.toNat!
  IO.println (n + 1)
