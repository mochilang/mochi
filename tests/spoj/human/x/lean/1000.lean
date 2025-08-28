/- Solution for SPOJ EQDIV - Equidivisions
https://www.spoj.com/problems/EQDIV/
-/

import Std
open Std

@[inline] def neighbors (n i j : Nat) : List (Nat × Nat) :=
  let mut acc : List (Nat × Nat) := []
  if i + 1 < n then acc := (i + 1, j) :: acc
  if j + 1 < n then acc := (i, j + 1) :: acc
  if i > 0 then acc := (i - 1, j) :: acc
  if j > 0 then acc := (i, j - 1) :: acc
  acc

def readGrid (h : IO.FS.Stream) (n : Nat) : IO (Array (Array Nat)) := do
  let mut grid := Array.replicate n (Array.replicate n 0)
  for region in [1:n] do
    let line := (← h.getLine).trim
    let parts := line.split (· = ' ') |>.filter (· ≠ "")
    let mut idx := 0
    while idx + 1 < parts.length do
      let x := parts[idx]!.toNat! - 1
      let y := parts[idx + 1]!.toNat! - 1
      let row := grid[x]!
      grid := grid.set! x (row.set! y region)
      idx := idx + 2
  for i in [0:n] do
    let row := grid[i]!
    let mut newRow := row
    for j in [0:n] do
      if row[j]! == 0 then
        newRow := newRow.set! j n
    grid := grid.set! i newRow
  return grid

def isConnected (grid : Array (Array Nat)) (n region : Nat) : Bool :=
  let cells :=
    Id.run do
      let mut cs : List (Nat × Nat) := []
      for i in [0:n] do
        for j in [0:n] do
          if grid[i]![j]! == region then
            cs := (i, j) :: cs
      return cs
  match cells with
  | [] => false
  | (start :: _) =>
      let total := cells.length
      let rec bfs (queue : List (Nat × Nat)) (seen : Std.HashSet (Nat × Nat)) : Std.HashSet (Nat × Nat) :=
        match queue with
        | [] => seen
        | (i, j) :: qs =>
            let nbrs := neighbors n i j
            let (qs, seen) :=
              nbrs.foldl
                (fun (qsSeen : List (Nat × Nat) × Std.HashSet (Nat × Nat)) (p : Nat × Nat) =>
                  let (qs, seen) := qsSeen
                  let (a, b) := p
                  if grid[a]![b]! == region && !seen.contains p then
                    ((a, b) :: qs, seen.insert p)
                  else
                    (qs, seen))
                (qs, seen)
            bfs qs seen
      let visited := bfs [start] (Std.HashSet.empty.insert start)
      visited.size == total

partial def process (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    let grid ← readGrid h n
    let mut ok := true
    for r in [1:n+1] do
      if ok && !isConnected grid n r then
        ok := false
    IO.println (if ok then "good" else "wrong")
    process h

def main : IO Unit := do
  let h ← IO.getStdin
  process h
