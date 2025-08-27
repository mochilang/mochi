/- Solution for SPOJ HARDQ - Hard Question
https://www.spoj.com/problems/HARDQ/
-/

import Std
open Std

structure Point where
  x : Int
  y : Int
  deriving Repr, Inhabited

private def orientation (a b c : Point) : Int :=
  (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

private def onSegment (a b c : Point) : Bool :=
  orientation a b c = 0 ∧
  Int.min a.x b.x ≤ c.x ∧ c.x ≤ Int.max a.x b.x ∧
  Int.min a.y b.y ≤ c.y ∧ c.y ≤ Int.max a.y b.y

private def properIntersect (a b c d : Point) : Bool :=
  let o1 := orientation a b c
  let o2 := orientation a b d
  let o3 := orientation c d a
  let o4 := orientation c d b
  ((o1 > 0 ∧ o2 < 0) ∨ (o1 < 0 ∧ o2 > 0)) ∧
  ((o3 > 0 ∧ o4 < 0) ∨ (o3 < 0 ∧ o4 > 0))

private def pointOnPolyEdge (p : Point) (poly : Array Point) : Bool :=
  Id.run do
    let mut res := false
    let n := poly.size
    for i in [0:n] do
      let a := poly.get! i
      let b := poly.get! ((i+1) % n)
      if onSegment a b p then
        res := true
    return res

private def pointInPoly (p : Point) (poly : Array Point) : Bool :=
  if pointOnPolyEdge p poly then true else
  Id.run do
    let px : Float := p.x.toFloat
    let py : Float := p.y.toFloat
    let n := poly.size
    let mut inside := false
    for i in [0:n] do
      let j := (i+1) % n
      let xi := poly.get! i |>.x.toFloat
      let yi := poly.get! i |>.y.toFloat
      let xj := poly.get! j |>.x.toFloat
      let yj := poly.get! j |>.y.toFloat
      if (yi > py) ≠ (yj > py) then
        let xint := xi + (xj - xi) * (py - yi) / (yj - yi)
        if px < xint then
          inside := !inside
    return inside

private def area2 (poly : Array Point) : Nat :=
  let n := poly.size
  let mut s : Int := 0
  for i in [0:n] do
    let j := (i+1) % n
    let a := poly.get! i
    let b := poly.get! j
    s := s + (a.x * b.y - b.x * a.y)
  Int.natAbs s

private def rectArea2 (x1 y1 x2 y2 : Int) : Nat :=
  let w := Int.natAbs (x1 - x2)
  let h := Int.natAbs (y1 - y2)
  (2 * w * h : Nat)

private def rectInside (poly : Array Point) (x1 y1 x2 y2 : Int) : Bool :=
  let minX := Int.min x1 x2
  let maxX := Int.max x1 x2
  let minY := Int.min y1 y2
  let maxY := Int.max y1 y2
  let p1 := Point.mk minX minY
  let p2 := Point.mk minX maxY
  let p3 := Point.mk maxX maxY
  let p4 := Point.mk maxX minY
  if ¬ pointInPoly p1 poly || ¬ pointInPoly p2 poly || ¬ pointInPoly p3 poly || ¬ pointInPoly p4 poly then
    false
  else
    let edges := #[(p1,p2),(p2,p3),(p3,p4),(p4,p1)]
    let n := poly.size
    Id.run do
      let mut bad := false
      for (a,b) in edges do
        for i in [0:n] do
          let c := poly.get! i
          let d := poly.get! ((i+1) % n)
          if properIntersect a b c d then
            bad := true
      pure (!bad)

private partial def solve (toks : Array String) (i : Nat) (acc : List String) : List String :=
  if i ≥ toks.size then acc.reverse else
  let n := toks.get! i |>.toNat!
  let m := toks.get! (i+1) |>.toNat!
  if n = 0 ∧ m = 0 then acc.reverse else
  let mut idx := i + 2
  let mut poly : Array Point := Array.mkEmpty n
  for _ in [0:n] do
    let x := toks.get! idx |>.toInt!
    let y := toks.get! (idx+1) |>.toInt!
    poly := poly.push {x := x, y := y}
    idx := idx + 2
  let polyArea := area2 poly
  let mut sum : Nat := 0
  for _ in [0:m] do
    let x1 := toks.get! idx |>.toInt!
    let y1 := toks.get! (idx+1) |>.toInt!
    let x2 := toks.get! (idx+2) |>.toInt!
    let y2 := toks.get! (idx+3) |>.toInt!
    idx := idx + 4
    if rectInside poly x1 y1 x2 y2 then
      sum := sum + rectArea2 x1 y1 x2 y2
  let ans := if sum = polyArea then "YES" else "NO"
  solve toks idx (ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let res := solve toks 0 []
  for s in res do
    IO.println s
