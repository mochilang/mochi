/- Solution for SPOJ QKP - Queens, Knights and Pawns
https://www.spoj.com/problems/QKP/
-/

import Std
open Std

/-- Parse a line describing pieces in the format `k r1 c1 r2 c2 ...` -/
def parsePieces (line : String) : Array (Nat × Nat) :=
  let parts := line.trim.split (· = ' ')
  let nums := parts.toArray.map (·.toNat!)
  let k := nums[0]!
  let mut res : Array (Nat × Nat) := #[]
  for i in [0:k] do
    let r := nums[1 + 2*i]!
    let c := nums[1 + 2*i + 1]!
    res := res.push (r, c)
  res

/-- Compute number of safe squares on the board. -/
def solveCase (n m : Nat) (queens knights pawns : Array (Nat × Nat)) : Nat :=
  Id.run do
    let mut occ : Std.HashSet (Nat × Nat) := {}
    for p in queens do occ := occ.insert p
    for p in knights do occ := occ.insert p
    for p in pawns do occ := occ.insert p
    let mut unsafe := occ
    let dirs : List (Int × Int) :=
      [ (1,0), (-1,0), (0,1), (0,-1),
        (1,1), (1,-1), (-1,1), (-1,-1) ]
    let nI : Int := Int.ofNat n
    let mI : Int := Int.ofNat m
    for (r,c) in queens do
      for (dr,dc) in dirs do
        let mut nr : Int := Int.ofNat r + dr
        let mut nc : Int := Int.ofNat c + dc
        while true do
          if decide (nr < 1 ∨ nr > nI ∨ nc < 1 ∨ nc > mI) then
            break
          let pos : Nat × Nat := (nr.toNat, nc.toNat)
          unsafe := unsafe.insert pos
          if occ.contains pos then
            break
          nr := nr + dr
          nc := nc + dc
    let knightMoves : List (Int × Int) :=
      [ (2,1), (2,-1), (-2,1), (-2,-1),
        (1,2), (1,-2), (-1,2), (-1,-2) ]
    for (r,c) in knights do
      for (dr,dc) in knightMoves do
        let nr := Int.ofNat r + dr
        let nc := Int.ofNat c + dc
        if decide (1 ≤ nr ∧ nr ≤ nI ∧ 1 ≤ nc ∧ nc ≤ mI) then
          unsafe := unsafe.insert (nr.toNat, nc.toNat)
    return n*m - unsafe.size

partial def process (h : IO.FS.Stream) (idx : Nat) : IO Unit := do
  let line ← h.getLine
  let parts := line.trim.split (· = ' ')
  let arr := parts.toArray
  let n := arr[0]!.toNat!
  let m := arr[1]!.toNat!
  if n == 0 ∧ m == 0 then
    pure ()
  else
    let qLine ← h.getLine
    let kLine ← h.getLine
    let pLine ← h.getLine
    let queens := parsePieces qLine
    let knights := parsePieces kLine
    let pawns := parsePieces pLine
    let safe := solveCase n m queens knights pawns
    IO.println s!"Board {idx} has {safe} safe squares."
    process h (idx+1)

def main : IO Unit := do
  process (← IO.getStdin) 1
