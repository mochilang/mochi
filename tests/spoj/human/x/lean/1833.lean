/- Solution for SPOJ SUDOKU2 - Sudoku
https://www.spoj.com/problems/SUDOKU2/
-/

import Std
open Std

abbrev Board := Array (Array Nat)

-- all permutations of [0,1,2]
def perm3 : Array (Array Nat) := #[
  #[0,1,2], #[0,2,1], #[1,0,2], #[1,2,0], #[2,0,1], #[2,1,0]
]

-- build full row/column permutation from segment and inner permutations
def buildMap (seg p0 p1 p2 : Array Nat) : Array Nat := Id.run do
  let mut res : Array Nat := Array.mkEmpty 9
  for r in [0:9] do
    let s := seg.get! (r / 3)
    let inner :=
      if s = 0 then p0.get! (r % 3)
      else if s = 1 then p1.get! (r % 3)
      else p2.get! (r % 3)
    res := res.push (s * 3 + inner)
  return res

-- rotate board k times clockwise
def rotate (b : Board) (k : Nat) : Board := Id.run do
  let mut res : Board := Array.mkEmpty 9
  for i in [0:9] do
    let mut row : Array Nat := Array.mkEmpty 9
    for j in [0:9] do
      let (r0,c0) :=
        match k % 4 with
        | 0 => (i, j)
        | 1 => (8 - j, i)
        | 2 => (8 - i, 8 - j)
        | _ => (j, 8 - i)
      let v := b.get! r0 |>.get! c0
      row := row.push v
    res := res.push row
  return res

-- check if uns board matches transformed board with given row/col maps
def consistent (b u : Board) (rm cm : Array Nat) : Bool := Id.run do
  let mut mp : Array Nat := Array.mkArray 10 0
  let mut inv : Array Nat := Array.mkArray 10 0
  for r in [0:9] do
    for c in [0:9] do
      let d := u.get! r |>.get! c
      if d != 0 then
        let x := b.get! (rm.get! r) |>.get! (cm.get! c)
        let m := mp.get! x
        if m = 0 then
          let v := inv.get! d
          if v != 0 ∧ v ≠ x then
            return false
          mp := mp.set! x d
          inv := inv.set! d x
        else if m ≠ d then
          return false
  return true

-- enumerate all transformations
def matchBoards (sol uns : Board) : Bool := Id.run do
  for rot in [0:4] do
    let b := rotate sol rot
    for seg in perm3 do
    for p0 in perm3 do
    for p1 in perm3 do
    for p2 in perm3 do
      let rm := buildMap seg p0 p1 p2
      for cseg in perm3 do
      for q0 in perm3 do
      for q1 in perm3 do
      for q2 in perm3 do
        let cm := buildMap cseg q0 q1 q2
        if consistent b uns rm cm then
          return true
  return false

-- read a 9x9 board
def readBoard (h : IO.FS.Stream) : IO Board := do
  let mut board : Board := Array.mkEmpty 9
  for _ in [0:9] do
    let line ← h.getLine
    let line := line.trim
    let mut row : Array Nat := Array.mkEmpty 9
    for ch in line.data do
      row := row.push (ch.toNat - '0'.toNat)
    board := board.push row
  return board

def solveIO : IO Unit := do
  let h ← IO.getStdin
  let n := (← h.getLine).trim.toNat!
  for t in [0:n] do
    let sol ← readBoard h
    let uns ← readBoard h
    if t + 1 < n then
      discard <| h.getLine -- consume blank line
    let ans := if matchBoards sol uns then "Yes" else "No"
    IO.println ans

def main : IO Unit := solveIO
