/- Solution for SPOJ IMAGE - Image Perimeters
https://www.spoj.com/problems/IMAGE/
-/
import Std
open Std

private def perimeter (grid : Array (Array Char)) (sr sc : Nat) : Nat := Id.run do
  let R := grid.size
  let C := grid[0]!.size
  let dirs4 : List (Int × Int) := [(-1,0),(1,0),(0,-1),(0,1)]
  let dirs8 : List (Int × Int) := [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
  let mut q : Array (Nat × Nat) := #[(sr, sc)]
  let mut idx : Nat := 0
  let mut vis : Std.HashSet (Nat × Nat) := {}
  vis := vis.insert (sr, sc)
  let mut per : Nat := 0
  while idx < q.size do
    let (r,c) := q[idx]!
    idx := idx + 1
    for (dr,dc) in dirs4 do
      let nr := Int.ofNat r + dr
      let nc := Int.ofNat c + dc
      if nr < 0 || nr ≥ Int.ofNat R || nc < 0 || nc ≥ Int.ofNat C then
        per := per + 1
      else
        let nrn := Int.toNat nr
        let ncn := Int.toNat nc
        if (grid.get! nrn).get! ncn = '.' then
          per := per + 1
    for (dr,dc) in dirs8 do
      let nr := Int.ofNat r + dr
      let nc := Int.ofNat c + dc
      if nr ≥ 0 && nr < Int.ofNat R && nc ≥ 0 && nc < Int.ofNat C then
        let nrn := Int.toNat nr
        let ncn := Int.toNat nc
        if (grid.get! nrn).get! ncn = 'X' && !vis.contains (nrn,ncn) then
          vis := vis.insert (nrn,ncn)
          q := q.push (nrn,ncn)
  return per

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line := (← h.getLine).trim
  if line = "" then
    loop h
  else
    let parts := line.split (· = ' ') |>.filter (· ≠ "")
    let r := parts[0]!.toNat!
    let c := parts[1]!.toNat!
    let sr := parts[2]!.toNat!
    let sc := parts[3]!.toNat!
    if r = 0 && c = 0 && sr = 0 && sc = 0 then
      pure ()
    else
      let mut grid : Array (Array Char) := Array.mkArray r #[]
      for i in [0:r] do
        let row := (← h.getLine).trim
        grid := grid.set! i row.data.toArray
      IO.println (perimeter grid (sr-1) (sc-1))
      loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
