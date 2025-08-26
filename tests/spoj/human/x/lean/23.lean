/- Solution for SPOJ PIR - Pyramids
https://www.spoj.com/problems/PIR/
-/

import Std
open Std

-- compute volume of tetrahedron from its six edge lengths
private def tetraVolume (ab ac ad bc bd cd : Float) : Float :=
  let W := ab
  let V := ac
  let u := ad
  let U := bc
  let v := bd
  let w := cd
  let U2 := U*U
  let V2 := V*V
  let W2 := W*W
  let u2 := u*u
  let v2 := v*v
  let w2 := w*w
  let u1 := v2 + w2 - U2
  let v1 := w2 + u2 - V2
  let w1 := u2 + v2 - W2
  let term := 4.0*u2*v2*w2 - u2*u1*u1 - v2*v1*v1 - w2*w1*w1 + u1*v1*w1
  let term := if term < 0 then 0 else term
  Float.sqrt term / 12.0

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
  | [ab, ac, ad, bc, bd, cd] => format4 (tetraVolume ab ac ad bc bd cd)
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
