/- Solution for SPOJ EMPTY - Empty Cuboids
https://www.spoj.com/problems/EMPTY/
-/

import Std
open Std

structure Point where
  x : Nat
  y : Nat
  z : Nat

def insertSorted (p : Nat × Nat) : List (Nat × Nat) → List (Nat × Nat)
| [] => [p]
| h :: t =>
    if p.fst < h.fst then p :: h :: t
    else if p.fst = h.fst then
      if p.snd < h.snd then p :: t else h :: t
    else
      h :: insertSorted p t

def bestYZSorted (sorted : List (Nat × Nat)) (limit : Nat) : (Nat × Nat × Nat) :=
  let init : Nat × Nat × Nat × Nat := (0, 0, 0, limit)
  let (bestA, bestY, bestZ, minZ) :=
    sorted.foldl
      (fun acc p =>
        let (bestA, bestY, bestZ, minZ) := acc
        let (y, z) := p
        let yCand := Nat.min (y - 1) limit
        let (bestA, bestY, bestZ) :=
          if yCand > 0 then
            let area := yCand * minZ
            if area > bestA then (area, yCand, minZ) else (bestA, bestY, bestZ)
          else (bestA, bestY, bestZ)
        let minZ := Nat.min minZ z
        (bestA, bestY, bestZ, minZ)
      ) init
  let area := limit * minZ
  let (bestA, bestY, bestZ) :=
    if area > bestA then (area, limit, minZ) else (bestA, bestY, bestZ)
  (bestA, bestY, bestZ)

def solveCase (pts : List Point) : (Nat × Nat × Nat) :=
  let limit := 1000000
  let sortedX := pts.qsort (fun a b => a.x < b.x)
  let init : List (Nat × Nat) × Nat × Nat × Nat × Nat := ([], 0, 0, 0, 0)
  let (S, bestX, bestY, bestZ, bestV) :=
    sortedX.foldl
      (fun state p =>
        let (S, bestX, bestY, bestZ, bestV) := state
        let xCand := Nat.min (p.x - 1) limit
        let (area, yCand, zCand) := bestYZSorted S limit
        let vol := xCand * area
        let (bestX, bestY, bestZ, bestV) :=
          if vol > bestV then (xCand, yCand, zCand, vol) else (bestX, bestY, bestZ, bestV)
        let S := insertSorted (p.y, p.z) S
        (S, bestX, bestY, bestZ, bestV)
      ) init
  let (area, yCand, zCand) := bestYZSorted S limit
  let vol := limit * area
  let (bestX, bestY, bestZ) :=
    if vol > bestV then (limit, yCand, zCand) else (bestX, bestY, bestZ)
  (bestX, bestY, bestZ)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t) do
    let n := (← h.getLine).trim.toNat!
    let mut pts : List Point := []
    for _ in [0:n) do
      let parts := (← h.getLine).trim.splitOn " "
      let x := (parts.get! 0).toNat!
      let y := (parts.get! 1).toNat!
      let z := (parts.get! 2).toNat!
      pts := {x := x, y := y, z := z} :: pts
    let (bx, by, bz) := solveCase pts
    IO.println s!"{bx} {by} {bz}"
