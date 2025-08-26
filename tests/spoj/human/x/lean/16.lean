/- Solution for SPOJ TETRA - Sphere in a tetrahedron
https://www.spoj.com/problems/TETRA
-/

import Std
open Std

-- compute area of triangle using Heron's formula
private def triArea (a b c : Float) : Float :=
  let s := (a + b + c) / 2.0
  Float.sqrt (s * (s - a) * (s - b) * (s - c))

-- compute inradius from edge lengths
private def inRadius (a b c d e f : Float) : Float :=
  let u := (a*a + b*b - d*d) / (2.0 * a)
  let v := Float.sqrt (b*b - u*u)
  let x := (a*a + c*c - e*e) / (2.0 * a)
  let y := (b*b + c*c - f*f - 2.0*u*x) / (2.0 * v)
  let z := Float.sqrt (c*c - x*x - y*y)
  let vol := Float.abs (a * v * z) / 6.0
  let s := triArea a b d + triArea a c e + triArea b c f + triArea d e f
  3.0 * vol / s

-- format float with four decimal places
private def format4 (x : Float) : String :=
  let y := x + 0.00005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "0000").take 4
    else
      "0000"
  intPart ++ "." ++ fracPart

private def solveLine (line : String) : String :=
  let nums := line.split (fun c => c = ' ')
                |>.filter (fun t => t ≠ "")
                |>.map (fun t => t.toFloat!)
  match nums with
  | [a,b,c,d,e,f] => format4 (inRadius a b c d e f)
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
