/- Solution for SPOJ FOLD - Fold
https://www.spoj.com/problems/FOLD/
-/

import Std
open Std

/-- Ceiling of log2 for positive n. -/
def ceilLog2 (n : Nat) : Nat :=
  let rec loop (k pow : Nat) : Nat :=
    if pow >= n then k else loop (k + 1) (pow * 2)
  loop 0 1

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let line ← h.getLine
    let s := line.trim
    if s.isEmpty then
      process h
    else
      let m := s.length + 1
      IO.println (ceilLog2 m)
      process h

def main : IO Unit :=
  process (← IO.getStdin)
