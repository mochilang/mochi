/- Solution for SPOJ CYLINDER - Cylinder
https://www.spoj.com/problems/CYLINDER/
-/

import Std
open Std

private def piF : Float := 3.141592653589793

private def format3 (x : Float) : String :=
  let y := x + 0.0005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts[0]!
  let fracPart :=
    if _ : parts.length > 1 then
      let f := parts[1]!
      (f ++ "000").take 3
    else
      "000"
  intPart ++ "." ++ fracPart

private def fmin (a b : Float) := if a < b then a else b
private def fmax (a b : Float) := if a < b then b else a

private def bestVolume (w h : Float) : Float :=
  let r1 := fmin (w / 2.0) (h / (2.0 * (piF + 1.0)))
  let v1 := piF * r1 * r1 * w
  let r2 := fmin (h / 3.0) (w / (2.0 * piF))
  let v2 := piF * r2 * r2 * (h - 2.0 * r2)
  fmax v1 v2

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let parts := line.trim.split (fun c => c = ' ') |>.filter (· ≠ "")
  if parts.length < 2 then
    pure ()
  else
    let w := parts[0]!.toNat!
    let hh := parts[1]!.toNat!
    if w == 0 && hh == 0 then
      pure ()
    else
      let v := bestVolume (Float.ofNat w) (Float.ofNat hh)
      IO.println (format3 v)
      loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
