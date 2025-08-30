/- Solution for SPOJ POLEVAL - Evaluate the polynomial
https://www.spoj.com/problems/POLEVAL/
-/

import Std
open Std

-- Evaluate polynomial at point x using Horner's method
def evalPoly (coeffs : List Int) (x : Int) : Int :=
  coeffs.foldl (fun acc c => acc * x + c) 0

partial def loop (h : IO.FS.Stream) (case : Nat) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toInt!
  if n = -1 then
    pure ()
  else
    let coeffLine ← h.getLine
    let coeffs := coeffLine.trim.splitOn " " |>.map String.toInt!
    let k := (← h.getLine).trim.toNat!
    let xLine ← h.getLine
    let xs := xLine.trim.splitOn " " |>.map String.toInt!
    IO.println s!"Case {case}:"
    for x in xs do
      IO.println (evalPoly coeffs x)
    loop h (case+1)

def main : IO Unit := do
  loop (← IO.getStdin) 1
