/- Solution for SPOJ TETRIS3D - Tetris 3D
https://www.spoj.com/problems/TETRIS3D/
-/

import Std
open Std

/-- parse space-separated natural numbers from a line -/
def readInts (line : String) : Array Nat :=
  line.trim.splitOn " " |>.filter (· ≠ "") |>.map (·.toNat!) |>.toArray

/-- simulate dropping blocks and report maximum height -/
def main : IO Unit := do
  let h ← IO.getStdin
  let first ← h.getLine
  let parts := readInts first
  let D := parts[0]!
  let S := parts[1]!
  let N := parts[2]!
  let mut grid : Array (Array Nat) := Array.mkArray S (Array.mkArray D 0)
  let mut best : Nat := 0
  for _ in [0:N] do
    let line ← h.getLine
    let ps := readInts line
    let d := ps[0]!; let s := ps[1]!; let w := ps[2]!; let x := ps[3]!; let y := ps[4]!
    let mut base : Nat := 0
    for yy in [y:y+s] do
      let row := grid[yy]!
      for xx in [x:x+d] do
        let h0 := row[xx]!
        if h0 > base then base := h0
    let nh := base + w
    if nh > best then best := nh
    for yy in [y:y+s] do
      let mut row := grid[yy]!
      for xx in [x:x+d] do
        row := row.set! xx nh
      grid := grid.set! yy row
  IO.println best
