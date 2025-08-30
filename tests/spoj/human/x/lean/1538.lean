/- Solution for SPOJ MKJUMPS - Making Jumps
https://www.spoj.com/problems/MKJUMPS/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (List Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.foldr (fun p acc => if p.length = 0 then acc else p.toNat! :: acc) []

/-- Knight move offsets. --/
def moves : List (Int × Int) :=
  [(-2,-1), (-2,1), (-1,-2), (-1,2), (1,-2), (1,2), (2,-1), (2,1)]

/-- Check if coordinates are inside the board and correspond to a usable square. --/
def isValid (board : Array (Array Bool)) (r c : Int) : Bool :=
  if r < 0 || c < 0 then
    false
  else
    let rn := Int.toNat r
    let cn := Int.toNat c
    if h : rn < board.size then
      if h' : cn < board[rn]!.size then
        board[rn]![cn]
      else
        false
    else
      false

/-- Generate reachable neighbors from a position. --/
def neighbors (board : Array (Array Bool)) (r c : Nat) : List (Nat × Nat) :=
  let ri := Int.ofNat r
  let ci := Int.ofNat c
  moves.foldl (fun acc (dr,dc) =>
    let nr := ri + dr
    let nc := ci + dc
    if isValid board nr nc then
      (nr.toNat, nc.toNat) :: acc
    else acc) []

/-- BFS count of squares reachable by the knight. --/
partial def bfs (board : Array (Array Bool)) (start : Nat × Nat) : Nat :=
  let rec loop (q : List (Nat × Nat)) (vis : Std.HashSet (Nat × Nat)) (cnt : Nat) : Nat :=
    match q with
    | [] => cnt
    | p :: qs =>
      if vis.contains p then
        loop qs vis cnt
      else
        let vis := vis.insert p
        let neigh := neighbors board p.fst p.snd
        loop (qs ++ neigh) vis (cnt + 1)
  loop [start] ({} : Std.HashSet (Nat × Nat)) 0

/-- Build boolean board array from row specifications. --/
def buildBoard (rows : Array (Nat × Nat)) : Array (Array Bool) :=
  Id.run do
    let n := rows.size
    let mut board : Array (Array Bool) := Array.replicate n (Array.replicate 10 false)
    for i in [0:n] do
      let (skip,len) := rows[i]!
      let mut row := board[i]!
      for j in [0:len] do
        row := row.set! (skip + j) true
      board := board.set! i row
    return board

/-- Process all cases from a list of integers. --/
partial def solveCases : Nat → List Nat → List String
| _, [] => []
| case, 0 :: _ => []
| case, n :: rest =>
  let rec takePairs : Nat → List Nat → List (Nat × Nat) × List Nat
  | 0, l => ([], l)
  | k+1, a :: b :: t =>
      let (ps, rem) := takePairs k t
      ((a, b) :: ps, rem)
  | _, l => ([], l)
  let (pairs, remaining) := takePairs n rest
  let arr := pairs.reverse.toArray
  let board := buildBoard arr
  let total := arr.foldl (fun s p => s + p.snd) 0
  let startCol := (arr[0]!).fst
  let reachable := bfs board (0, startCol)
  let unreachable := total - reachable
  let word := if unreachable == 1 then "square" else "squares"
  let line := s!"Case {case}, {unreachable} {word} can not be reached."
  line :: solveCases (case + 1) remaining

/-- Main entry point. --/
def main : IO Unit := do
  let data ← readInts
  let lines := solveCases 1 data
  for l in lines do
    IO.println l
