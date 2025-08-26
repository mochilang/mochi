/- Solution for SPOJ STONE - Lifting the Stone
https://www.spoj.com/problems/STONE/
-/

import Std
open Std

private def format2 (x : Float) : String :=
  let absx := if x < 0.0 then -x else x
  if absx < 0.005 then
    "0.00"
  else
    let y := if x ≥ 0.0 then x + 0.005 else x - 0.005
    let s := y.toString
    let parts := s.splitOn "."
    let intPart :=
      match parts with
      | p :: _ => if p == "-0" then "0" else p
      | [] => "0"
    let fracPart :=
      match parts.drop 1 with
      | f :: _ => (f ++ "00").take 2
      | [] => "00"
    intPart ++ "." ++ fracPart

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let line0 := (← h.getLine).trim
    let ps0 := line0.split (fun c => c = ' ')
                  |>.filter (· ≠ "")
    let (sx0, sy0) :=
      match ps0 with
      | sx :: sy :: _ => (sx, sy)
      | _ => ("0", "0")
    let x0 := Float.ofInt (sx0.toInt!)
    let y0 := Float.ofInt (sy0.toInt!)
    let mut prevx := x0
    let mut prevy := y0
    let mut area2 : Float := 0.0
    let mut cx : Float := 0.0
    let mut cy : Float := 0.0
    for _ in [:n-1] do
      let line := (← h.getLine).trim
      let ps := line.split (fun c => c = ' ')
                 |>.filter (· ≠ "")
      let (sx, sy) :=
        match ps with
        | sx :: sy :: _ => (sx, sy)
        | _ => ("0", "0")
      let x := Float.ofInt (sx.toInt!)
      let y := Float.ofInt (sy.toInt!)
      let cross := prevx * y - x * prevy
      area2 := area2 + cross
      cx := cx + (prevx + x) * cross
      cy := cy + (prevy + y) * cross
      prevx := x
      prevy := y
    let cross := prevx * y0 - x0 * prevy
    area2 := area2 + cross
    cx := cx + (prevx + x0) * cross
    cy := cy + (prevy + y0) * cross
    let gx := cx / (3.0 * area2)
    let gy := cy / (3.0 * area2)
    IO.println (format2 gx ++ " " ++ format2 gy)
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
