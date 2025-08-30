-- https://www.spoj.com/problems/LEMON/
import Std
open Std

-- approximate solution using sampling
-- S samples along height and ground to achieve ~0.01 precision

abbrev S : Nat := 2000

-- read a line of floats
def readFloats : IO (List Float) := do
  let line ← IO.getLine
  pure <| line.trim.split (· = ' ').filter (· ≠ "").map String.toFloat!

-- compute area for one test case
def solveCase (n : Nat) (alpha : Float) (hs : List Float) (rs : List Float) : IO Float := do
  let cot := 1.0 / Float.tan alpha
  let totalH := hs.foldl (·+·) 0.0
  let maxr := rs.foldl (fun m r => if r > m then r else m) 0.0
  let xMin := -maxr
  let xMax := totalH * cot + maxr
  let stepX := (xMax - xMin) / Float.ofNat S
  -- cumulative heights
  let mut cum : Array Float := #[0.0]
  for h in hs do
    cum := cum.push (cum.back! + h)
  -- precompute radius and centers
  let mut rVals : Array Float := Array.mkEmpty (S+1)
  let mut xCenters : Array Float := Array.mkEmpty (S+1)
  let mut idx : Nat := 0
  for j in [0:S+1] do
    let z := totalH * Float.ofNat j / Float.ofNat S
    -- move idx to correct interval
    while idx+1 < cum.size && z > cum.get! (idx+1) do
      idx := idx + 1
    let r :=
      if idx >= rs.length then
        0.0
      else
        let r0 := rs.get! idx
        let r1 := if idx+1 < rs.length then rs.get! (idx+1) else 0.0
        let h := hs.get! idx
        let t := if h = 0.0 then 0.0 else (z - cum.get! idx) / h
        r0 + (r1 - r0) * t
    rVals := rVals.push r
    xCenters := xCenters.push (z * cot)
  -- integrate across x
  let mut area : Float := 0.0
  for j in [0:S] do
    let x := xMin + Float.ofNat j * stepX
    let mut best : Float := 0.0
    for k in [0:S+1] do
      let xc := xCenters.get! k
      let r := rVals.get! k
      let v := r*r - (x - xc)*(x - xc)
      if v > best then best := v
    if best > 0.0 then
      area := area + 2.0 * Float.sqrt best * stepX
  return area

@[main]
def main : IO Unit := do
  let t ← (← IO.getLine).trim.toNat!
  for _ in [0:t] do
    let line1 ← IO.getLine
    let parts := line1.trim.split (· = ' ').filter (· ≠ "")
    let n := parts.head!.toNat!
    let alpha := parts.tail!.head!.toFloat!
    let hline ← readFloats
    let rline ← readFloats
    let hs := hline.tail! -- drop h0
    let area ← solveCase n alpha hs rline
    IO.println (String.formatFloatFixed area 2)
