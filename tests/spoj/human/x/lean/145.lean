/- Solution for SPOJ ALIENS - Aliens
https://www.spoj.com/problems/ALIENS/
-/

import Std
open Std

structure Point where
  x : Float
  y : Float

structure Circle where
  c : Point
  r : Float

private def distSq (a b : Point) : Float :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  dx*dx + dy*dy

private def circleTwo (a b : Point) : Circle :=
  let c := Point.mk ((a.x + b.x)/2.0) ((a.y + b.y)/2.0)
  let r := Float.sqrt (distSq c a)
  {c := c, r := r}

private def circleThree (a b c : Point) : Circle :=
  let d := 2.0 * (a.x*(b.y - c.y) + b.x*(c.y - a.y) + c.x*(a.y - b.y))
  if Float.abs d < 1e-12 then
    circleTwo a b
  else
    let ax2 := a.x*a.x + a.y*a.y
    let bx2 := b.x*b.x + b.y*b.y
    let cx2 := c.x*c.x + c.y*c.y
    let ux := (ax2*(b.y - c.y) + bx2*(c.y - a.y) + cx2*(a.y - b.y)) / d
    let uy := (ax2*(c.x - b.x) + bx2*(a.x - c.x) + cx2*(b.x - a.x)) / d
    let cen := Point.mk ux uy
    {c := cen, r := Float.sqrt (distSq cen a)}

private def inCircle (circ : Circle) (p : Point) : Bool :=
  distSq circ.c p ≤ circ.r*circ.r + 1e-7

private def shuffle (pts : Array Point) : IO (Array Point) := do
  let n := pts.size
  let mut a := pts
  for i in [0:n] do
    let j ← IO.rand i (n-1)
    let pi := a.get! i
    let pj := a.get! j
    a := a.set! i pj
    a := a.set! j pi
  return a

private def minimalCircle (pts0 : Array Point) : IO Circle := do
  let pts ← shuffle pts0
  let n := pts.size
  let mut c : Circle := {c := {x := 0.0, y := 0.0}, r := -1.0}
  for i in [0:n] do
    let p := pts.get! i
    if c.r < 0.0 || !inCircle c p then
      c := {c := p, r := 0.0}
      for j in [0:i] do
        let q := pts.get! j
        if !inCircle c q then
          c := circleTwo p q
          for k in [0:j] do
            let r := pts.get! k
            if !inCircle c r then
              c := circleThree p q r
  return c

def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (toks.get! idx).toNat!
    let mut i := idx + 1
    let mut pts : Array Point := #[]
    for _ in [0:n] do
      let x := (toks.get! i).toFloat!
      let y := (toks.get! (i+1)).toFloat!
      pts := pts.push ⟨x, y⟩
      i := i + 2
    let circ ← minimalCircle pts
    IO.println (format2 circ.r)
    IO.println (format2 circ.c.x ++ " " ++ format2 circ.c.y)
    solve toks i (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let t := (toks.get! 0).toNat!
  solve toks 1 t
