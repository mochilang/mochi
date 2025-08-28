/- Solution for SPOJ TFOSS - Fossil in the Ice
https://www.spoj.com/problems/TFOSS/
-/
import Std
open Std

structure Pt where
  x : Int
  y : Int
  deriving Inhabited

/-- twice signed area of triangle oab -/
private def cross (o a b : Pt) : Int :=
  (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

/-- squared distance between two points -/
private def dist2 (a b : Pt) : Int :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  dx * dx + dy * dy

/-- convex hull via monotonic chain, keeping boundary points -/
private def convexHull (pts : Array Pt) : Array Pt :=
  Id.run do
    let sorted := pts.qsort (fun a b =>
      if a.x == b.x then a.y < b.y else a.x < b.x)
    let mut lower : Array Pt := #[]
    for p in sorted do
      while lower.size ≥ 2 && cross (lower[lower.size-2]!) (lower[lower.size-1]!) p < 0 do
        lower := lower.pop
      lower := lower.push p
    let mut upper : Array Pt := #[]
    for p in sorted.reverse do
      while upper.size ≥ 2 && cross (upper[upper.size-2]!) (upper[upper.size-1]!) p < 0 do
        upper := upper.pop
      upper := upper.push p
    lower := lower.pop
    upper := upper.pop
    return lower.append upper

/-- diameter of a convex polygon using rotating calipers -/
private def hullDiameter (h : Array Pt) : Int :=
  if h.size ≤ 1 then 0 else
    Id.run do
      let n := h.size
      let mut j := 1
      let mut best : Int := 0
      for i in [0:n] do
        let ni := (i + 1) % n
        let mut cont := true
        while cont do
          let nj := (j + 1) % n
          if cross (h[i]!) (h[ni]!) (h[nj]!) > cross (h[i]!) (h[ni]!) (h[j]!) then
            j := nj
          else
            cont := false
        best := max best (dist2 h[i]! h[j]!)
        best := max best (dist2 h[ni]! h[j]!)
      return best

private def solveCase (pts : Array Pt) : Int :=
  let hull := convexHull pts
  hullDiameter hull

partial def process (tokens : Array String) (idx t : Nat) : IO Unit := do
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
    IO.println (solveCase pts)
    process tokens j (t - 1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |>.toArray
  let t := (tokens[0]!).toNat!
  process tokens 1 t
