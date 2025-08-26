/- Solution for SPOJ SHORTCUT - Shortcut
https://www.spoj.com/problems/SHORTCUT/
-/

import Std
open Std

structure Pt where
  x : Int
  y : Int
  idx : Nat
deriving Inhabited

structure Best where
  len : Nat
  b : Nat
  e : Nat
  dir : Char

/-- Solve a single test case given the movement string. -/
private def solveCase (moves : String) : String :=
  Id.run do
    -- build coordinates of break points
    let mut coords : Array (Int × Int) := #[(0,0)]
    let mut x : Int := 0
    let mut y : Int := 0
    for c in moves.data do
      match c with
      | 'N' => y := y + 1
      | 'S' => y := y - 1
      | 'E' => x := x + 1
      | 'W' => x := x - 1
      | _   => pure ()
      coords := coords.push (x, y)

    -- points with index
    let mut pts : Array Pt := Array.mkEmpty coords.size
    for i in [0:coords.size] do
      let (cx, cy) := coords.get! i
      pts := pts.push {x := cx, y := cy, idx := i}

    let update (bst : Best) (i j : Nat) : Best :=
      let b := if i ≤ j then i else j
      let e := if i ≤ j then j else i
      if e == b + 1 then
        bst
      else
        let (xb, yb) := coords.get! b
        let (xe, ye) := coords.get! e
        let l :=
          if xb == xe then Int.natAbs (ye - yb)
          else Int.natAbs (xe - xb)
        let dir :=
          if xb == xe then
            if ye > yb then 'N' else 'S'
          else if xe > xb then 'E' else 'W'
        if l < bst.len || (l == bst.len && (b < bst.b || (b == bst.b && e > bst.e))) then
          {len := l, b := b, e := e, dir := dir}
        else
          bst

    let mut best : Best := {len := Nat.succ moves.length, b := 0, e := 0, dir := 'N'}

    -- vertical candidates
    let mut sortedX := pts.qsort (fun a b => if a.x == b.x then a.y < b.y else a.x < b.x)
    for k in [1:sortedX.size] do
      let p1 := sortedX.get! (k-1)
      let p2 := sortedX.get! k
      if p1.x == p2.x then
        best := update best p1.idx p2.idx

    -- horizontal candidates
    let mut sortedY := pts.qsort (fun a b => if a.y == b.y then a.x < b.x else a.y < b.y)
    for k in [1:sortedY.size] do
      let p1 := sortedY.get! (k-1)
      let p2 := sortedY.get! k
      if p1.y == p2.y then
        best := update best p1.idx p2.idx

    s!"{best.len} {best.b} {best.e} {best.dir}"

/-- Main entry point: parse input and output results. -/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let t := (← stdin.getLine).trim.toNat!
  for _ in [0:t] do
    let _n := (← stdin.getLine).trim.toNat!
    let moves := (← stdin.getLine).trim
    IO.println (solveCase moves)
