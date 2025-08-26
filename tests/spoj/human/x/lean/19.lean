/- Solution for SPOJ CRYPTO3 - The Bytelandian Cryptographer (Act III)
https://www.spoj.com/problems/CRYPTO3/
-/

import Std
open Std

def main : IO Unit := do
  let data ← IO.readStdin
  let n := data.trim.toNat!
  let u := n * 2
  IO.println u
