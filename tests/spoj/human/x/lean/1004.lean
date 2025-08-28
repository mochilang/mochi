-- https://www.spoj.com/problems/POLYCODE/
import Std
open Std


-- Cantor unpairing function
def unpair (z : Nat) : Nat × Nat :=
  let s := Nat.sqrt (8 * z + 1)
  let w := (s - 1) / 2
  let t := w * (w + 1) / 2
  let y := z - t
  let x := w - y
  (x, y)

-- decode n-tuple encoded via nested Cantor pairing
def decodeTuple : Nat → Nat → List Nat
| 0, _ => []
| 1, z => [z]
| Nat.succ (Nat.succ k), z =>
  let (a, rest) := unpair z
  a :: decodeTuple (Nat.succ k) rest

def decodePolygon (z : Nat) : List (Nat × Nat) :=
  let (n, rest) := unpair z
  let codes := decodeTuple n rest
  codes.map unpair

-- compute shoelace sum (twice area)
def shoelace2 : List (Int × Int) → Int
| [] => 0
| p :: ps =>
  let rec aux (prev : Int × Int) (first : Int × Int) : List (Int × Int) → Int → Int
  | [], acc =>
      let (px, py) := prev
      let (fx, fy) := first
      acc + px * fy - py * fx
  | q :: qs, acc =>
      let (px, py) := prev
      let (qx, qy) := q
      aux q first qs (acc + px * qy - py * qx)
  aux p p ps 0

def areaString (pts : List (Nat × Nat)) : String :=
  let ptsInt := pts.map (fun p => (Int.ofNat p.fst, Int.ofNat p.snd))
  let s := shoelace2 ptsInt
  let a := Int.natAbs s
  let q := a / 2
  let r := a % 2
  if r = 0 then s!"{q}.0" else s!"{q}.5"

partial def mainLoop : IO Unit := do
  let line ← IO.getLine
  let s := line.trim
  if s = "*" then
    pure ()
  else
    match s.toNat? with
    | none => pure ()
    | some n =>
      let pts := decodePolygon n
      IO.println (areaString pts)
      mainLoop

def main : IO Unit := mainLoop
