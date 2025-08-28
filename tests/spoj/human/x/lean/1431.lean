/- Solution for SPOJ KPPOLY - Projections Of A Polygon
https://www.spoj.com/problems/KPPOLY/
-/

import Std
open Std

/-- compute w+h for given rotation angle --/
def measure (pts : Array (Float × Float)) (th : Float) : Float :=
  let c := Float.cos th
  let s := Float.sin th
  let big := 1e30
  let mut minx := big
  let mut maxx := -big
  let mut miny := big
  let mut maxy := -big
  for p in pts do
    let px := p.fst
    let py := p.snd
    let projx := px * c + py * s
    let projy := -px * s + py * c
    if projx < minx then minx := projx
    if projx > maxx then maxx := projx
    if projy < miny then miny := projy
    if projy > maxy then maxy := projy
  (maxx - minx) + (maxy - miny)

/-- solve for min and max sum of projections --/
def solve (pts : Array (Float × Float)) : String :=
  let n := pts.size
  let mut angles : Array Float := #[0.0]
  for i in [0:n] do
    for j in [i+1:n] do
      let dx := pts[j]!.fst - pts[i]!.fst
      let dy := pts[j]!.snd - pts[i]!.snd
      let mut th := Float.atan2 dy dx
      if th < 0.0 then th := th + Float.pi
      if th >= Float.pi / 2.0 then th := th - Float.pi / 2.0
      angles := angles.push th
  let mut mn := 1e30
  let mut mx := 0.0
  for th in angles do
    let v := measure pts th
    if v < mn then mn := v
    if v > mx then mx := v
  s!"{mn} {mx}"

/-- main: read input and output results --/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
  let arr := Array.ofList toks
  let n := (arr.get! 0).toNat!
  let mut pts : Array (Float × Float) := #[]
  let mut idx := 1
  for _ in [0:n] do
    let x := Float.ofInt ((arr.get! idx).toInt!)
    let y := Float.ofInt ((arr.get! (idx+1)).toInt!)
    pts := pts.push (x, y)
    idx := idx + 2
  IO.println (solve pts)
