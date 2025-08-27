/- Solution for SPOJ PERIOD - Periodic Strings
https://www.spoj.com/problems/PERIOD/
-/

import Std
open Std

/-- Build prefix-function (KMP failure function) for string as array of chars. -/
private def buildPi (s : Array Char) : Array Nat :=
  let n := s.size
  let rec loop (i len : Nat) (pi : Array Nat) : Array Nat :=
    if i ≥ n then
      pi
    else if s.get! i == s.get! len then
      let len := len + 1
      let pi := pi.set! i len
      loop (i + 1) len pi
    else if len ≠ 0 then
      loop i (pi.get! (len - 1)) pi
    else
      let pi := pi.set! i 0
      loop (i + 1) 0 pi
  loop 1 0 (Array.mkArray n 0)

/-- Process all test cases. --/
partial def process (h : IO.FS.Stream) (case t : Nat) : IO Unit := do
  if case > t then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let s := (← h.getLine).trim
    IO.println s!"Test case #{case}"
    let arr := s.data.toArray
    let pi := buildPi arr
    for i in [2:n+1] do
      let p := i - pi.get! (i - 1)
      if i % p == 0 then
        let k := i / p
        if k > 1 then
          IO.println s!"{i} {k}"
    IO.println ""
    process h (case + 1) t

/-- Main entry point. --/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h 1 t
