/- Solution for SPOJ FCTRL2 - Small factorials
https://www.spoj.com/problems/FCTRL2/
-/

import Std
open Std

def fact : Nat -> Nat
| 0 => 1
| (n+1) => (n+1) * fact n

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    let m := line.trim.toNat!
    IO.println (fact m)
    loop h (n - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
