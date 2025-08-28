/- Solution for SPOJ COMCB - Complete Chess Boards
https://www.spoj.com/problems/COMCB/
-/

import Std
open Std

-- Convert a board position to its string representation like "A1"
def posStr (r c : Nat) : String :=
  let col := Char.ofNat (65 + c)
  let row := toString (r + 1)
  String.singleton col ++ row

-- Generate all valid knight moves from (r, c)
def knightMoves (R C r c : Nat) : List (Nat × Nat) :=
  let offsets : List (Int × Int) :=
    [(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1)]
  let R' : Int := Int.ofNat R
  let C' : Int := Int.ofNat C
  let r' : Int := Int.ofNat r
  let c' : Int := Int.ofNat c
  offsets.filterMap fun (dr, dc) =>
    let nr := r' + dr
    let nc := c' + dc
    if 0 ≤ nr && nr < R' && 0 ≤ nc && nc < C' then
      some (nr.toNat, nc.toNat)
    else
      none

partial def dfs (R C : Nat) (r c : Nat) (visited : Std.HashSet (Nat × Nat))
    (path : String) (step total : Nat) : Option String :=
  if step == total then
    some path
  else
    let movesArr := (knightMoves R C r c).toArray
    let movesArr := movesArr.qsort (fun a b => posStr a.1 a.2 < posStr b.1 b.2)
    let rec tryMoves (ms : List (Nat × Nat)) : Option String :=
      match ms with
      | [] => none
      | m :: rest =>
        if visited.contains m then
          tryMoves rest
        else
          let visited' := visited.insert m
          let path' := path ++ posStr m.1 m.2
          match dfs R C m.1 m.2 visited' path' (step+1) total with
          | some res => some res
          | none => tryMoves rest
    tryMoves movesArr.toList

def solveBoard (R C : Nat) : String :=
  let total := R * C
  match dfs R C 0 0 ((Std.HashSet.emptyWithCapacity 1).insert (0,0)) (posStr 0 0) 1 total with
  | some s => s
  | none => "-1"

partial def process (h : IO.FS.Stream) (cases : Nat) : IO Unit := do
  if cases = 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h cases
    else
      let parts := line.split (· = ' ') |>.filter (· ≠ "")
      let x := parts[0]!.toNat!
      let y := parts[1]!.toNat!
      IO.println (solveBoard x y)
      process h (cases - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
