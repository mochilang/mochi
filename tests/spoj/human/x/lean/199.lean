/- Solution for SPOJ EMPTY - Empty Cuboids
https://www.spoj.com/problems/EMPTY/
-/
import Std
open Std

/-- Read all whitespace-separated tokens from stdin. --/
def readTokens : IO (Array String) := do
  let s ← IO.readStdin
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (· ≠ "") |>.toArray

/-- Compute the best y, z and their product for the given active points.
`pts` holds pairs (y, z) for points with x already fixed. --/
partial def computeBestYZ (pts : Array (Nat × Nat)) : Nat × Nat × Nat := Id.run do
  let limit : Nat := 1000000
  if pts.isEmpty then
    return (limit * limit, limit, limit)
  else
    let sorted := pts.qsort (fun a b => a.fst < b.fst)
    let mut minZ : Nat := limit
    let mut bestVol : Nat := 0
    let mut bestY : Nat := 1
    let mut bestZ : Nat := 1
    for (y, z) in sorted do
      let vol := y * minZ
      if vol > bestVol then
        bestVol := vol
        bestY := y
        bestZ := minZ
      if z < minZ then
        minZ := z
    let vol := limit * minZ
    if vol > bestVol then
      bestVol := vol
      bestY := limit
      bestZ := minZ
    return (bestVol, bestY, bestZ)

/-- Solve single test case given array of points. --/
def solveCase (pts : Array (Nat × Nat × Nat)) : String := Id.run do
  let limit : Nat := 1000000
  let sorted := pts.qsort (fun a b => a.fst < b.fst)
  let mut xs : List Nat := sorted.toList.map (fun p => p.fst)
  xs := (limit :: xs).qsort (· < ·)
  xs := xs.dedup
  let mut idx : Nat := 0
  let mut active : Array (Nat × Nat) := #[]
  let mut bestVol : Nat := 0
  let mut bestX : Nat := 1
  let mut bestY : Nat := 1
  let mut bestZ : Nat := 1
  for x in xs do
    -- add all points with xi < x
    while h : idx < sorted.size && sorted.get! idx |>.fst < x do
      let (_, y, z) := sorted.get! idx
      active := active.push (y, z)
      idx := idx + 1
    let (yzVol, yBest, zBest) := computeBestYZ active
    let vol := x * yzVol
    if vol > bestVol then
      bestVol := vol
      bestX := x
      bestY := yBest
      bestZ := zBest
  return s!"{bestX} {bestY} {bestZ}"

/-- Main IO entry point. --/
def main : IO Unit := do
  let tokens ← readTokens
  let t := tokens.get! 0 |>.toNat!
  let mut idx := 1
  let mut outs : Array String := #[]
  for _ in [0:t] do
    let n := tokens.get! idx |>.toNat!
    idx := idx + 1
    let mut pts : Array (Nat × Nat × Nat) := #[]
    for _ in [0:n] do
      let x := tokens.get! idx |>.toNat!
      let y := tokens.get! (idx+1) |>.toNat!
      let z := tokens.get! (idx+2) |>.toNat!
      idx := idx + 3
      pts := pts.push (x, y, z)
    outs := outs.push (solveCase pts)
  IO.println (String.intercalate "\n" outs)
