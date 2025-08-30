/- Solution for SPOJ PEARL - The Game of Pearls
https://www.spoj.com/problems/PEARL/
-/

import Std
open Std

abbrev Point := Int × Int
abbrev Shape := List (Nat × Nat)

def rot (p : Point) : Point := (p.snd, -p.fst)
def mirror (p : Point) : Point := (p.fst, -p.snd)

def normalize (ps : List Point) : Shape :=
  let minR := ps.foldl (fun m p => Int.min m p.fst) 0
  let minC := ps.foldl (fun m p => Int.min m p.snd) 0
  ps.map (fun (r,c) => ((r - minR).toNat, (c - minC).toNat))

def orientations (base : Shape) : List Shape :=
  let pts := base.map (fun (r,c) => (r.toInt, c.toInt))
  let rots :=
    [0,1,2,3].map (fun k =>
      (List.range k).foldl (fun s _ => s.map rot) pts)
  let flipped := rots.map (fun s => s.map mirror)
  let all := rots ++ flipped
  let allN := all.map normalize
  allN.foldl (fun acc s => if acc.any (· = s) then acc else s :: acc) []
    |> List.reverse

structure Piece :=
  name : Char
  forms : List Shape
  size  : Nat

def basePieces : List (Char × Shape) := [
  ('A', [(0,0),(1,0),(1,1)]),
  ('B', [(0,0),(1,0),(2,0),(3,0)]),
  ('C', [(0,0),(0,1),(0,2),(1,2)]),
  ('D', [(0,0),(0,1),(1,0),(1,1)]),
  ('E', [(0,0),(0,1),(0,2),(1,0),(2,0)]),
  ('F', [(0,2),(1,0),(1,1),(1,2),(1,3)]),
  ('G', [(0,0),(0,2),(1,0),(1,1),(1,2)]),
  ('H', [(0,1),(0,2),(1,0),(1,1),(1,2)]),
  ('I', [(0,1),(1,0),(1,1),(2,0),(3,0)]),
  ('J', [(0,1),(1,0),(1,1),(1,2),(2,1)]),
  ('K', [(0,0),(1,0),(1,1),(2,1),(2,2)]),
  ('L', [(0,0),(1,0),(1,1),(1,2),(1,3)])
]

def allPieces : List Piece :=
  basePieces.map (fun (p) =>
    { name := p.fst,
      forms := orientations p.snd,
      size := p.snd.length })

abbrev Board := Array (Array Char)

def emptyBoard : Board :=
  let rows := List.range 10
  rows.map (fun r => (List.replicate (r+1) '.').toArray) |>.toArray

def inBoard (r c : Nat) : Bool := c ≤ r ∧ r < 10

def prefilled (b : Board) : Std.HashMap Char (List (Nat×Nat)) :=
  let mut m : Std.HashMap Char (List (Nat×Nat)) := {}
  for r in [0:10] do
    let row := b[r]!
    for c in [0:row.size] do
      let ch := row[c]!
      if ch ≠ '.' then
        let lst := m.findD ch []
        m := m.insert ch ((r,c)::lst)
  m

partial def place (b : Board) (pieces : List Piece)
    (pf : Std.HashMap Char (List (Nat×Nat))) : Option Board :=
  match pieces with
  | [] => some b
  | p :: rest =>
    let targets := pf.findD p.name []
    let rec tryForms (fs : List Shape) : Option Board :=
      match fs with
      | [] => none
      | f :: fs' =>
        let rec tryPos (r : Nat) (c : Nat) : Option Board :=
          if r = 10 then none else
            let cells := f.map (fun (dr,dc) => (r+dr, c+dc))
            let ok := cells.all (fun (rr,cc) => inBoard rr cc ∧
              let ch := b[rr]![cc]!
              (ch = '.' ∨ ch = p.name))
            let cover := targets.all (fun t => cells.contains t)
            let next :=
              if ok ∧ cover then
                let b' := cells.foldl (fun bb (rr,cc) =>
                  bb.set! rr ((bb[rr]!).set! cc p.name)) b
                place b' rest pf
              else none
            match next with
            | some ans => some ans
            | none =>
              let (r', c') :=
                if c+1 ≤ r then (r, c+1) else (r+1, 0)
              tryPos r' c'
        match tryPos 0 0 with
        | some ans => some ans
        | none => tryForms fs'
    tryForms p.forms

partial def solveBoard (b : Board) : Option Board :=
  let pf := prefilled b
  let pieces := allPieces.filter (fun p =>
    let used := pf.findD p.name []
    used.length ≠ p.size)
  place b pieces pf

def readBoard (lines : List String) : Board :=
  lines.enum.map (fun ⟨r,s⟩ =>
    let chars := s.data.toArray
    chars) |>.toArray

def boardToString (b : Board) : List String :=
  (List.range 10).map (fun r => (List.range (r+1)).map (fun c => b[r]![c]!).asString)

partial def solveAll (ls : List String) : List String :=
  let rec loop (n : Nat) (xs : List String) (acc : List String) :=
    if n = 0 then acc.reverse else
      let (boardLines, rest) := xs.splitAt 10
      let b := readBoard boardLines
      match solveBoard b with
      | some res =>
          let out := boardToString res
          loop (n-1) rest (out ++ acc)
      | none => loop (n-1) rest ("No solution" :: acc)
  loop 10 ls []

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let lines := input.splitOn "\n" |>.filter (·.length > 0)
  let outLines := solveAll lines
  IO.println (String.intercalate "\n" outLines)
