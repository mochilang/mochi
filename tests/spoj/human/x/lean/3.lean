/- Solution for SPOJ SBSTR1 - Substring Check (Bug Funny)
https://www.spoj.com/problems/SBSTR1/
-/

import Std
open Std

/-- Check if string `b` is a substring of `a`. -/
private def isSubstr (a b : String) : Bool :=
  let la := a.length
  let lb := b.length
  let rec loop (i : Nat) : Bool :=
    if i + lb > la then
      false
    else if a.extract i (i + lb) == b then
      true
    else
      loop (i + 1)
  loop 0

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
      match line.splitOn " " with
      | a :: b :: _ =>
        let res := if isSubstr a b then "1" else "0"
        IO.println res
        process h
      | _ => pure ()

def main : IO Unit := do
  process (← IO.getStdin)
