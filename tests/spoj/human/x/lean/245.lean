/- Solution for SPOJ SQRROOT - Square Root
https://www.spoj.com/problems/SQRROOT/
-/

import Std
open Std

-- parse a decimal string into Nat without size limit
private def parseNat (s : String) : Nat :=
  s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let n := parseNat line.trim
    IO.println (Nat.sqrt n)
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := parseNat (← h.getLine).trim
  loop h t
