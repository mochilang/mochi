/- Solution for SPOJ SUMFOUR - 4 values whose sum is 0
https://www.spoj.com/problems/SUMFOUR/
-/

import Std
open Std

private def readInts (line : String) : List Int :=
  line.split (· = ' ') |>.filter (fun s => s ≠ "") |>.map String.toInt!

def main : IO Unit := do
  let stdin ← IO.getStdin
  let n := (← stdin.getLine).trim.toNat!
  let mut A : Array Int := Array.mkEmpty n
  let mut B : Array Int := Array.mkEmpty n
  let mut C : Array Int := Array.mkEmpty n
  let mut D : Array Int := Array.mkEmpty n
  for _ in [0:n] do
    let nums := readInts (← stdin.getLine)
    match nums with
    | [a,b,c,d] =>
        A := A.push a
        B := B.push b
        C := C.push c
        D := D.push d
    | _ => pure ()
  let mut m : Std.HashMap Int Nat := {}
  for a in A do
    for b in B do
      let s := a + b
      let c := m.findD s 0
      m := m.insert s (c + 1)
  let mut ans : Nat := 0
  for c in C do
    for d in D do
      let target := -(c + d)
      match m.find? target with
      | some cnt => ans := ans + cnt
      | none => pure ()
  IO.println (toString ans)
