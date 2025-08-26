/- Solution for SPOJ MUL - Fast Multiplication
https://www.spoj.com/problems/MUL/
-/

import Std
open Std

-- multiply two arbitrarily large non-negative integers given on a line
private def mulLine (line : String) : String :=
  let nums := line.split (fun c => c = ' ')
                |>.filter (fun s => s ≠ "")
  match nums with
  | [a, b] =>
      let n1 := a.toNat!
      let n2 := b.toNat!
      toString (n1 * n2)
  | _      => ""

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    IO.println (mulLine line.trim)
    loop h (n - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
