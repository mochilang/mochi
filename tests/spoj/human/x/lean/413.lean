/- Solution for SPOJ WPUZZLES - Word Puzzles
https://www.spoj.com/problems/WPUZZLES/
-/

import Std
open Std

def dirs : Array (Int × Int × Char) := #[
  (-1,0,'A'), (-1,1,'B'), (0,1,'C'), (1,1,'D'),
  (1,0,'E'), (1,-1,'F'), (0,-1,'G'), (-1,-1,'H')
]

partial def matchAt (grid : Array (Array Char)) (rows cols : Nat)
    (word : Array Char) (r c dr dc : Int) : Bool :=
  let rowsI : Int := rows
  let colsI : Int := cols
  let rec go (i : Nat) (rr cc : Int) : Bool :=
    if i == word.size then true
    else if rr < 0 || rr ≥ rowsI || cc < 0 || cc ≥ colsI then false
    else
      let ch := (grid[rr.toNat]!)[cc.toNat]!
      if ch ≠ word[i]! then false
      else go (i+1) (rr+dr) (cc+dc)
  go 0 r c

def findWord (grid : Array (Array Char))
    (posMap : Std.HashMap Char (Array (Nat × Nat)))
    (rows cols : Nat) (wordStr : String) : (Nat × Nat × Char) :=
  Id.run do
    let word := wordStr.toList.toArray
    let first := word[0]!
    let candidates := match Std.HashMap.get? posMap first with
      | some arr => arr
      | none => #[]
    let mut res : Option (Nat × Nat × Char) := none
    for (r,c) in candidates do
      if res.isSome then pure () else
      for (dr,dc,ch) in dirs do
        if res.isNone && matchAt grid rows cols word (r:Int) (c:Int) dr dc then
          res := some (r,c,ch)
    return res.getD (0,0,'A')

def buildMap (grid : Array (Array Char)) (rows cols : Nat)
    : Std.HashMap Char (Array (Nat × Nat)) :=
  Id.run do
    let mut mp : Std.HashMap Char (Array (Nat × Nat)) := {}
    for r in [0:rows] do
      let row := grid[r]!
      for c in [0:cols] do
        let ch := row[c]!
        let arr := match Std.HashMap.get? mp ch with
          | some a => a
          | none => #[]
        mp := mp.insert ch (arr.push (r,c))
    return mp

partial def loop (tokens : Array String) (i : Nat) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let l := (tokens[i]!).toNat!
    let c := (tokens[i+1]!).toNat!
    let w := (tokens[i+2]!).toNat!
    let mut grid : Array (Array Char) := #[]
    for r in [0:l] do
      let line := tokens[i+3+r]!
      grid := grid.push line.toList.toArray
    let mp := buildMap grid l c
    let mut idx := i + 3 + l
    for _ in [0:w] do
      let word := tokens[idx]!
      let (r,c,ori) := findWord grid mp l c word
      IO.println s!"{r} {c} {ori}"
      idx := idx + 1
    if t > 1 then IO.println ""
    loop tokens idx (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let tokensList := (data.split (fun ch => ch = ' ' || ch = '\n' || ch = '\t' || ch = '\r'))
                     |> List.filter (fun s => s ≠ "")
  let tokens := tokensList.toArray
  let t := (tokens[0]!).toNat!
  loop tokens 1 t
