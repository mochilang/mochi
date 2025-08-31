/- Solution for SPOJ CPRMT - Common Permutation
https://www.spoj.com/problems/CPRMT/
-/
import Std
open Std

-- count frequency of each lowercase letter
private def freq (s : String) : Array Nat :=
  s.data.foldl
    (fun arr c =>
      let idx := c.toNat - 'a'.toNat
      arr.set! idx ((arr.get! idx) + 1))
    (Array.mkArray 26 0)

-- compute alphabetically sorted common letters
private def commonStr (a b : String) : String :=
  let fa := freq a
  let fb := freq b
  let chars := (List.range 26).bind (fun i =>
      let cnt := Nat.min (fa.get! i) (fb.get! i)
      let ch := Char.ofNat ('a'.toNat + i)
      List.replicate cnt ch)
  String.mk chars

partial def loop (h : IO.FS.Stream) : IO Unit := do
  if (← h.isEof) then
    pure ()
  else
    let a ← h.getLine
    if (← h.isEof) then
      pure ()
    else
      let b ← h.getLine
      IO.println (commonStr a.trim b.trim)
      loop h

def main : IO Unit := do
  loop (← IO.getStdin)
