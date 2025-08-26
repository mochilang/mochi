/- Solution for SPOJ EQBOX - Equipment Box
https://www.spoj.com/problems/EQBOX/
-/

import Std
open Std

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    if line.trim.isEmpty then
      loop h t
    else
      let parts := line.trim.splitOn " "
      let a := parts.get! 0 |>.toNat!
      let b := parts.get! 1 |>.toNat!
      let x := parts.get! 2 |>.toNat!
      let y := parts.get! 3 |>.toNat!
      let ok := (x < a && y < b) || (x < b && y < a)
      if ok then
        IO.println "Escape is possible."
      else
        IO.println "Box cannot be dropped."
      loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
