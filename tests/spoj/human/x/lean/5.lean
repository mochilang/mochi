/- Solution for SPOJ PALIN - The Next Palindrome
https://www.spoj.com/problems/PALIN
-/
import Std
open Std

def isPal (s : String) : Bool :=
  s.data == s.data.reverse

def nextPal (n : Nat) : Nat :=
  let rec loop (m : Nat) :=
    let v := m + 1
    if isPal (toString v) then v else loop v
  loop n

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (nextPal n)
    process h (t - 1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let tLine ← stdin.getLine
  let t := tLine.trim.toNat!
  process stdin t
