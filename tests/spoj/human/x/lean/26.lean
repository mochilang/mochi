/- Solution for SPOJ BSHEEP - Build the Fence
https://www.spoj.com/problems/BSHEEP/
-/

import Std
open Std

structure Pt where
  x : Int
  y : Int
  idx : Nat

def cross (o a b : Pt) : Int :=
  (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

def dist (a b : Pt) : Float :=
  let dx := (a.x - b.x).toFloat
  let dy := (a.y - b.y).toFloat
  Float.sqrt (dx*dx + dy*dy)

def uniquePts (pts : Array (Int × Int)) : Array Pt :=
  let mut map : Std.HashMap (Int × Int) Nat := {}
  let mut res : Array Pt := #[]
  for i in [0:pts.size] do
    let (x,y) := pts.get! i
    if map.contains (x,y) then
      pure ()
    else
      map := map.insert (x,y) (i+1)
      res := res.push {x:=x, y:=y, idx:=i+1}
  res

def convexHull (pts : Array Pt) : Array Pt :=
  let n := pts.size
  if n ≤ 1 then pts else
    let sorted := pts.qsort (fun a b =>
      if a.x == b.x then a.y < b.y else a.x < b.x)
    let mut lower : Array Pt := #[]
    for p in sorted do
      let rec shrink (l : Array Pt) : Array Pt :=
        if l.size ≥ 2 && cross (l.get! (l.size-2)) (l.get! (l.size-1)) p ≤ 0 then
          shrink (l.pop)
        else l
      lower := shrink lower
      lower := lower.push p
    let mut upper : Array Pt := #[]
    for i in [0:sorted.size] do
      let p := sorted.get! (sorted.size - 1 - i)
      let rec shrink (u : Array Pt) : Array Pt :=
        if u.size ≥ 2 && cross (u.get! (u.size-2)) (u.get! (u.size-1)) p ≤ 0 then
          shrink (u.pop)
        else u
      upper := shrink upper
      upper := upper.push p
    let lower := lower.pop
    let upper := upper.pop
    lower.append upper

def perimeter (h : Array Pt) : Float :=
  if h.size ≤ 1 then 0.0 else
    Id.run do
      let mut s : Float := 0.0
      for i in [0:h.size] do
        let a := h.get! i
        let b := h.get! ((i+1) % h.size)
        s := s + dist a b
      return s

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

def solveCase (ptsIn : Array (Int × Int)) : String :=
  let pts := uniquePts ptsIn
  let hull := convexHull pts
  let per := perimeter hull
  let start := Id.run do
    let mut s := 0
    for i in [1:hull.size] do
      let p := hull.get! i
      let q := hull.get! s
      if p.y < q.y || (p.y == q.y && p.x < q.x) then
        s := i
    return s
  let hull := Id.run do
    let mut res : Array Pt := #[]
    for i in [0:hull.size] do
      res := res.push (hull.get! ((start + i) % hull.size))
    return res
  let idxStr := String.intercalate " " (hull.toList.map (fun p => toString p.idx))
  format2 per ++ "\n" ++ idxStr

partial def loop (tokens : Array String) (i : Nat) (t : Nat)
    : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (tokens.get! i).toNat!
    let mut pts : Array (Int × Int) := #[]
    let mut j := i+1
    for _ in [0:n] do
      let x := (tokens.get! j).toInt!
      let y := (tokens.get! (j+1)).toInt!
      pts := pts.push (x,y)
      j := j + 2
    IO.println (solveCase pts)
    if t > 1 then IO.println ""
    loop tokens j (t-1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
  let t := (tokens.get! 0).toNat!
  loop tokens 1 t
