-- https://www.spoj.com/problems/RHOMBS/
import Std
open Std

structure Point where
  x : Int
  y : Int
  deriving Repr

def dirs : Array Point :=
  #[⟨1,0⟩, ⟨0,1⟩, ⟨-1,1⟩, ⟨-1,0⟩, ⟨0,-1⟩, ⟨1,-1⟩]

def inside (poly : Array Point) (px py : Int) : Bool :=
  let pxF := (Float.ofInt px)
  let pyF := (Float.ofInt py)
  let n := poly.size
  let mut j : Nat := n - 1
  let mut c := false
  for i in [0:n] do
    let pi := poly.get! i
    let pj := poly.get! j
    let yi := Float.ofInt pi.y
    let yj := Float.ofInt pj.y
    let xi := Float.ofInt pi.x
    let xj := Float.ofInt pj.x
    let cond1 := (yi > pyF) ≠ (yj > pyF)
    let cond2 := pxF < (xj - xi) * (pyF - yi) / (yj - yi) + xi
    if cond1 && cond2 then
      c := !c
    j := i
  c

def colorUp (x y : Int) : Int := Int.emod (x + 2*y) 3

def colorDown (x y : Int) : Int := Int.emod (x + 2*y + 1) 3

def processCase (edges : List (Int × Int)) : (Int × Int × Int) :=
  let mut pts : Array Point := #[⟨0,0⟩]
  let mut cx : Int := 0
  let mut cy : Int := 0
  for (d,k) in edges do
    let dir := dirs.get! (Nat.ofInt (d - 1))
    cx := cx + dir.x * k
    cy := cy + dir.y * k
    pts := pts.push ⟨cx,cy⟩
  -- bounding box
  let mut minX := pts.get! 0 |>.x
  let mut maxX := minX
  let mut minY := pts.get! 0 |>.y
  let mut maxY := minY
  for p in pts do
    if p.x < minX then minX := p.x
    if p.x > maxX then maxX := p.x
    if p.y < minY then minY := p.y
    if p.y > maxY then maxY := p.y
  let poly := pts.map (fun p => ⟨p.x * 3, p.y * 3⟩)
  let width := Nat.ofInt (maxX - minX + 1)
  let height := Nat.ofInt (maxY - minY + 1)
  let mut cnt : Array Int := #[0,0,0]
  for ix in [0:width] do
    let x := minX + Int.ofNat ix
    for iy in [0:height] do
      let y := minY + Int.ofNat iy
      -- up triangle center
      if inside poly (3*x + 1) (3*y + 1) then
        let c := colorUp x y
        cnt := cnt.modify (Nat.ofInt c) (· + 1)
      -- down triangle center
      if inside poly (3*x + 2) (3*y + 2) then
        let c := colorDown x y
        cnt := cnt.modify (Nat.ofInt c) (· + 1)
  let t0 := cnt.get! 0
  let t1 := cnt.get! 1
  let t2 := cnt.get! 2
  let a := (t0 + t1 - t2) / 2
  let b := (t1 + t2 - t0) / 2
  let c := (t2 + t0 - t1) / 2
  (a,b,c)

partial def readInts : IO (List Int) := do
  let line ← IO.getLine
  return line.trim.splitOn " ".map String.toInt!

partial def main : IO Unit := do
  let tLine ← IO.getLine
  let t := String.toNat! tLine.trim
  for _ in List.range t do
    let nLine ← IO.getLine
    let n := String.toNat! nLine.trim
    let mut edges : List (Int × Int) := []
    for _ in List.range n do
      let ints ← readInts
      edges := edges.concat [(ints.get! 0, ints.get! 1)]
    let (a,b,c) := processCase edges
    IO.println s!"{a} {b} {c}"
