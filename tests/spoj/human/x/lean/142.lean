/- Solution for SPOJ GLUE - Johnny and the Glue
https://www.spoj.com/problems/GLUE/
-/

import Std
open Std

structure Pt where
  x : Int
  y : Int
deriving Inhabited

/-- Lexicographic comparison of points. -/
private def cmpPt (a b : Pt) : Bool :=
  a.x < b.x ∨ (a.x == b.x ∧ a.y < b.y)

/-- Twice the signed area of triangle oab. -/
private def cross (o a b : Pt) : Int :=
  (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

/-- Convex hull including collinear boundary points. -/
private def convexHull (pts : Array Pt) : Array Pt :=
  Id.run do
    let sorted := pts.qsort cmpPt
    let mut lower : Array Pt := #[]
    for p in sorted do
      while lower.size ≥ 2 &&
          cross (lower[lower.size-2]! ) (lower[lower.size-1]! ) p < 0 do
        lower := lower.pop
      lower := lower.push p
    let mut upper : Array Pt := #[]
    for p in sorted.reverse do
      while upper.size ≥ 2 &&
          cross (upper[upper.size-2]! ) (upper[upper.size-1]! ) p < 0 do
        upper := upper.pop
      upper := upper.push p
    lower := lower.pop
    upper := upper.pop
    return lower.append upper

/-- Normalize direction vector to canonical form ignoring sign. -/
private def normDir (dx dy : Int) : Int × Int :=
  let g := Int.ofNat (Nat.gcd dx.natAbs dy.natAbs)
  let dx := dx / g
  let dy := dy / g
  let (dx, dy) :=
    if dx < 0 ∨ (dx == 0 ∧ dy < 0) then (-dx, -dy) else (dx, dy)
  (dx, dy)

/-- Count rectangles covering all points with each side touching ≥2 dabs. -/
private def countRectangles (pts : Array Pt) : Int :=
  if pts.isEmpty then
    0
  else
    let hull := convexHull pts
    if hull.size < 3 then
      0
    else
      Id.run do
        let mut mp : Std.HashMap (Int × Int) (Std.HashSet Int) := {}
        for i in [0:hull.size] do
          let a := hull[i]!
          let b := hull[(i+1) % hull.size]!
          let dir := normDir (b.x - a.x) (b.y - a.y)
          let nx := -dir.snd
          let ny := dir.fst
          let c := nx * a.x + ny * a.y
          let s := match mp[dir]? with
                   | some set => set.insert c
                   | none => (Std.HashSet.empty).insert c
          mp := mp.insert dir s
        let mut ans : Int := 0
        for (dir, s) in mp.toList do
          if s.size ≥ 2 then
            let (dx, dy) := dir
            let perp := normDir (-dy) dx
            if let some s2 := mp[perp]? then
              if s2.size ≥ 2 then
                ans := ans + 1
        return ans / 2

/-- Main program: read input and output counts. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |>.toArray
  let t := (tokens[0]!).toNat!
  let rec loop (idx t : Nat) : IO Unit := do
    if t = 0 then
      pure ()
    else
      let n := (tokens[idx]!).toNat!
      let mut pts : Array Pt := #[]
      let mut j := idx + 1
      for _ in [0:n] do
        let x := (tokens[j]!).toInt!
        let y := (tokens[j+1]!).toInt!
        pts := pts.push {x:=x, y:=y}
        j := j + 2
      IO.println (countRectangles pts)
      loop j (t - 1)
  loop 1 t
