/- Solution for SPOJ RATTERN - The Room Pattern
https://www.spoj.com/problems/RATTERN/
-/

import Std
open Std

structure Orientation where
  coords : Array (Int × Int)
  colors : Array Nat

structure Tile where
  cost : Nat
  orients : Array Orientation

def shapeCoords (form : Nat) : Array (Int × Int) :=
  match form with
  | 1 => #[(0,0)]
  | 2 => #[(0,0),(0,1)]
  | 3 => #[(0,0),(0,1),(1,0)]       -- L triomino
  | 4 => #[(0,0),(0,1),(0,2)]       -- straight triomino
  | _ => #[]

def rotate (coords : Array (Int × Int)) : Array (Int × Int) :=
  coords.map (fun (r,c) => (-c, r))

def genOrientations (coords : Array (Int × Int)) (colors : Array Nat) : Array Orientation :=
  let o0 : Orientation := {coords := coords, colors := colors}
  let o1 : Orientation := {coords := rotate o0.coords, colors := colors}
  let o2 : Orientation := {coords := rotate o1.coords, colors := colors}
  let o3 : Orientation := {coords := rotate o2.coords, colors := colors}
  #[o0, o1, o2, o3]

abbrev Memo := Std.HashMap Nat Nat

partial def dfs (mask goal n m : Nat) (colors : Array Nat) (tiles : Array Tile) : StateM Memo Nat := do
  let memo ← get
  if let some v := memo.find? mask then
    return v
  if mask == goal then
    modify (fun mp => mp.insert mask 0)
    return 0
  let rec first (i : Nat) : Nat :=
    if Nat.testBit mask i then first (i+1) else i
  let idx := first 0
  let r := idx / m
  let c := idx % m
  let nI := Int.ofNat n
  let mI := Int.ofNat m
  let total := n*m
  let mut best := (1000000000 : Nat)
  for tile in tiles do
    for ori in tile.orients do
      let len := ori.coords.size
      for anchor in [0:len] do
        let (dr,dc) := ori.coords.get! anchor
        let rowOff := Int.ofNat r - dr
        let colOff := Int.ofNat c - dc
        let rec place (j : Nat) (acc : Nat) : Option Nat :=
          if h : j < len then
            let (rr,cc) := ori.coords.get ⟨j, h⟩
            let rr := rowOff + rr
            let cc := colOff + cc
            if rr < 0 || rr ≥ nI || cc < 0 || cc ≥ mI then
              none
            else
              let idx2 := rr.toNat * m + cc.toNat
              if Nat.testBit acc idx2 || colors.get! idx2 ≠ ori.colors.get! j then
                none
              else
                place (j+1) (acc + Nat.pow 2 idx2)
          else
            some acc
        match place 0 mask with
        | some nm =>
            let cost ← dfs nm goal n m colors tiles
            let total := cost + tile.cost
            if total < best then
              best := total
        | none => pure ()
  modify (fun mp => mp.insert mask best)
  return best

partial def solveCases (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (toks.get! idx).toNat!
    let m := (toks.get! (idx+1)).toNat!
    let k := (toks.get! (idx+2)).toNat!
    let mut i := idx + 3
    let total := n*m
    let mut colors : Array Nat := Array.mkArray total 0
    let mut mask : Nat := 0
    for r in [0:n] do
      for c in [0:m] do
        let v := (toks.get! i).toNat!
        i := i + 1
        let id := r*m + c
        if v = 2 then
          mask := mask + Nat.pow 2 id
        else
          colors := colors.set! id v
    let mut tiles : Array Tile := #[]
    for _ in [0:k] do
      let form := (toks.get! i).toNat!
      let cost := (toks.get! (i+1)).toNat!
      i := i + 2
      let base := shapeCoords form
      let sz := base.size
      let mut cols : Array Nat := #[]
      for _ in [0:sz] do
        cols := cols.push ((toks.get! i).toNat!)
        i := i + 1
      let oris := genOrientations base cols
      tiles := tiles.push {cost := cost, orients := oris}
    let goal := Nat.pow 2 (n*m) - 1
    let ans := (dfs mask goal n m colors tiles).run' {}
    let out := if ans ≥ 1000000000 then -1 else (Int.ofNat ans)
    IO.println out
    solveCases toks i (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let t := (toks.get! 0).toNat!
  solveCases toks 1 t
