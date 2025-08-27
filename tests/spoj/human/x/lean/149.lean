/- Solution for SPOJ FSHEEP - Fencing in the Sheep
https://www.spoj.com/problems/FSHEEP/
-/

import Std
open Std

private def parseInts (line : String) : List Int :=
  line.trim.split (· = ' ') |>.filterMap (fun s =>
    if s.isEmpty then none else some s.toInt!)

private def findEdge (angles : Array Float) (theta : Float) : Nat :=
  let rec go (l r : Nat) : Nat :=
    if l + 1 = r then l
    else
      let m := (l + r) / 2
      if theta < angles[m]! then go l m else go m r
  go 0 (angles.size - 1)

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let ws := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let n := ws.get! 0 |>.toNat!
    let m := ws.get! 1 |>.toNat!
    let mut xs : Array Int := Array.mkEmpty n
    let mut ys : Array Int := Array.mkEmpty n
    let mut angs : Array Float := Array.mkEmpty (n + 1)
    for i in [:n] do
      let ln ← h.getLine
      let vals := parseInts ln
      let x := vals.get! 0
      let y := vals.get! 1
      xs := xs.push x
      ys := ys.push y
      let mut a := Float.atan2 (Float.ofInt y) (Float.ofInt x)
      if i > 0 then
        let prev := angs.back!
        while a < prev do
          a := a + (2.0 * Float.pi)
      angs := angs.push a
    angs := angs.push (angs[0]! + 2.0 * Float.pi)
    let mut ex : Array Int := Array.mkEmpty n
    let mut ey : Array Int := Array.mkEmpty n
    let mut ce : Array Int := Array.mkEmpty n
    for i in [:n] do
      let j := if i + 1 = n then 0 else i + 1
      ex := ex.push (xs[j]! - xs[i]!)
      ey := ey.push (ys[j]! - ys[i]!)
      ce := ce.push (xs[i]! * ys[j]! - ys[i]! * xs[j]!)
    let mut cnt : Nat := 0
    for _ in [:m] do
      let ln ← h.getLine
      let vals := parseInts ln
      let x := vals.get! 0
      let y := vals.get! 1
      let mut th := Float.atan2 (Float.ofInt y) (Float.ofInt x)
      let base := angs[0]!
      while th < base do th := th + (2.0 * Float.pi)
      let top := angs.back!
      while th >= top do th := th - (2.0 * Float.pi)
      let idx := findEdge angs th
      let c := x * ey[idx]! - y * ex[idx]!
      if c <= ce[idx]! then cnt := cnt + 1
    IO.println cnt
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
