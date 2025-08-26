/- Solution for SPOJ TRICENTR - Triangle From Centroid
https://www.spoj.com/problems/TRICENTR/
-/

import Std
open Std

private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

private def solveLine (line : String) : String :=
  let nums := line.split (fun c => c = ' ')
                |>.filter (· ≠ "")
                |>.map (·.toFloat!)
  match nums with
  | [a, ga, gb, gc] =>
      let area := 1.5 * a * ga
      let b := (2.0 * area) / (3.0 * gb)
      let c := (2.0 * area) / (3.0 * gc)
      let R := a * b * c / (4.0 * area)
      let og2 := Float.max 0.0 (R*R - (a*a + b*b + c*c) / 9.0)
      let hg := 2.0 * Float.sqrt og2
      format3 area ++ " " ++ format3 hg
  | _ => ""

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    IO.println (solveLine line.trim)
    loop h (n-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
