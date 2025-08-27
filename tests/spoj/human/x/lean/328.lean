/- Solution for SPOJ BISHOPS - Bishops
https://www.spoj.com/problems/BISHOPS/
-/

import Std
open Std

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let line ← h.getLine
    let line := line.trim
    if line.isEmpty then
      process h
    else
      let n := line.toNat!
      let ans := if n <= 1 then n else (2 * n - 2)
      IO.println ans
      process h

def main : IO Unit := do
  process (← IO.getStdin)
