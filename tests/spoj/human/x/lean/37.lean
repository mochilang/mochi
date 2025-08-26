/- Solution for SPOJ PROPKEY - The Proper Key
https://www.spoj.com/problems/PROPKEY/
-/

import Std
open Std

set_option linter.deprecated false

/-- Check if the key placed with its left edge at `x` and bottom depth `y`
collides with any wood in the lock. `y` is the distance from the top of the
lock to the bottom of the key. -/
def collides (keyCells : Array (Nat × Nat)) (lock : Array (Array Char))
    (R D W : Nat) (x y : Nat) : Bool :=
  let top : Int := Int.ofNat y - Int.ofNat R
  let dInt : Int := Int.ofNat D
  let mut bad := false
  for (i, j) in keyCells do
    if !bad then
      let yy : Int := top + Int.ofNat i
      if 0 <= yy && yy < dInt then
        let row := lock.get! yy.toNat
        if row.get! (x + j) == '#' then
          bad := true
  bad

/-- Breadth-first search over all reachable positions of the key. Returns
`(canFallThrough, maxDepth)` where `maxDepth` is the maximum bottom depth
reached before falling through. -/
def fallDepth (keyCells : Array (Nat × Nat)) (lock : Array (Array Char))
    (R C D W : Nat) : Bool × Nat :=
  let width := W - C + 1
  let stride := D + R + 1
  let total := width * stride
  let mut visited := Array.mkArray total false
  let mut queue : Array (Nat × Nat) := #[]
  -- initial states: key above lock at depth 0 for each horizontal position
  for x in [0:width] do
    queue := queue.push (x, 0)
    let idx := x * stride
    visited := visited.set! idx true
  let mut front := 0
  let mut maxD := 0
  let mut can := false
  while front < queue.size && !can do
    let (x, y) := queue.get! front
    front := front + 1
    if y >= D + R then
      can := true
    else
      if y > maxD then
        maxD := y
      -- try move left
      if x > 0 then
        let nx := x - 1
        let idx := nx * stride + y
        if !visited.get! idx && !collides keyCells lock R D W nx y then
          visited := visited.set! idx true
          queue := queue.push (nx, y)
      -- try move right
      if x + 1 < width then
        let nx := x + 1
        let idx := nx * stride + y
        if !visited.get! idx && !collides keyCells lock R D W nx y then
          visited := visited.set! idx true
          queue := queue.push (nx, y)
      -- try move down
      if y + 1 <= D + R then
        let ny := y + 1
        let idx := x * stride + ny
        if !visited.get! idx && !collides keyCells lock R D W x ny then
          visited := visited.set! idx true
          queue := queue.push (x, ny)
  (can, maxD)

/-- Process a single test case. -/
def solveCase (R C D W : Nat) (keyLines lockLines : Array String) : String :=
  -- extract key cells
  let mut cells : Array (Nat × Nat) := #[]
  for i in [0:R] do
    let row := (keyLines.get! i).data.toArray
    for j in [0:C] do
      if row.get! j == '#' then
        cells := cells.push (i, j)
  -- convert lock to array of char arrays
  let lock := lockLines.map (fun s => s.data.toArray)
  let (can, depth) := fallDepth cells lock R C D W
  if can then
    "The key can fall through."
  else
    s!"The key falls to depth {depth}."

partial def parseCases (toks : Array String) (idx t : Nat) (acc : Array String) : Array String :=
  if t = 0 then
    acc
  else
    let R := toks.get! idx |>.toNat!
    let C := toks.get! (idx + 1) |>.toNat!
    let mut i := idx + 2
    let mut keyLines : Array String := #[]
    for _ in [0:R] do
      keyLines := keyLines.push (toks.get! i)
      i := i + 1
    let D := toks.get! i |>.toNat!
    let W := toks.get! (i + 1) |>.toNat!
    i := i + 2
    let mut lockLines : Array String := #[]
    for _ in [0:D] do
      lockLines := lockLines.push (toks.get! i)
      i := i + 1
    let line := solveCase R C D W keyLines lockLines
    parseCases toks i (t - 1) (acc.push line)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (fun s => s ≠ "") |> Array.ofList
  let T := toks[0]!.toNat!
  let outs := parseCases toks 1 T #[]
  for line in outs do
    IO.println line
