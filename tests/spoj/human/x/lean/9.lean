/- Solution for SPOJ DIRVS - Direct Visibility
https://www.spoj.com/problems/DIRVS/
-/

import Std
open Std

private def get2D (a : Array (Array α)) (r c : Nat) : α := (a.get! r).get! c
private def set2D (a : Array (Array α)) (r c : Nat) (v : α) : Array (Array α) :=
  a.set! r ((a.get! r).set! c v)

private def natAbs (a b : Nat) : Nat := if a ≥ b then a - b else b - a

private def visible (grid : Array (Array Int)) (r c rb cb : Nat) : Bool :=
  let h1 := Float.ofInt (get2D grid r c) + 0.5
  let h2 := Float.ofInt (get2D grid rb cb) + 0.5
  let x1 := Float.ofNat c + 0.5
  let y1 := Float.ofNat r + 0.5
  let x2 := Float.ofNat cb + 0.5
  let y2 := Float.ofNat rb + 0.5
  let dx := x2 - x1
  let dy := y2 - y1
  let dz := h2 - h1
  let steps := 2 * Nat.max (natAbs rb r) (natAbs cb c) + 1
  let rec loop (i : Nat) : Bool :=
    if h : i < steps then
      let t := Float.ofNat i / Float.ofNat steps
      let x := x1 + dx * t
      let y := y1 + dy * t
      let z := h1 + dz * t
      let xi := Int.toNat (Float.floor x)
      let yi := Int.toNat (Float.floor y)
      if xi < grid[0]!.size && yi < grid.size then
        if z ≤ Float.ofInt (get2D grid yi xi) then
          false
        else
          loop (i+1)
      else
        loop (i+1)
    else
      true
  loop 1

partial def bfs (grid : Array (Array Int)) (P Q r1 c1 r2 c2 : Nat) : Option Nat :=
  let dirs : List (Int × Int) := [(1,0),(-1,0),(0,1),(0,-1)]
  let invalid := 1000000
  let mut dist := Array.mkArray P (Array.mkArray Q invalid)
  dist := set2D dist r1 c1 0
  let rec loop (queue : List (Nat × Nat)) (dist : Array (Array Nat)) : Option Nat :=
    match queue with
    | [] => if get2D dist r2 c2 == invalid then none else some (get2D dist r2 c2)
    | (r,c)::qs =>
      let d := get2D dist r c
      if r = r2 && c = c2 then some d
      else
        let (dist, qs) := dirs.foldl (fun (acc : Array (Array Nat) × List (Nat×Nat)) (dir : Int×Int) =>
          let (accDist, accQ) := acc
          let nr := Int.ofNat r + dir.fst
          let nc := Int.ofNat c + dir.snd
          if nr ≥ 0 && nr < Int.ofNat P && nc ≥ 0 && nc < Int.ofNat Q then
            let nrn := Int.toNat nr
            let ncn := Int.toNat nc
            let diff := get2D grid nrn ncn - get2D grid r c
            if diff ≤ 1 && diff ≥ -3 && get2D accDist nrn ncn > d+1 &&
               (visible grid nrn ncn r1 c1 || visible grid nrn ncn r2 c2) then
              let newDist := set2D accDist nrn ncn (d+1)
              (newDist, accQ ++ [(nrn,ncn)])
            else
              (accDist, accQ)
          else
            (accDist, accQ)
        ) (dist, qs) dirs
        loop qs dist
  loop [(r1,c1)] dist

partial def parseCases (toks : Array String) (idx : Nat) (t : Nat) (acc : List String) : List String :=
  if h : t = 0 then acc.reverse else
    let P := toks[idx]!.toNat!
    let Q := toks[idx+1]!.toNat!
    let mut i := idx + 2
    let mut grid : Array (Array Int) := Array.mkArray P (Array.mkArray Q 0)
    for r in [0:P] do
      let mut row := Array.mkArray Q 0
      for c in [0:Q] do
        row := row.set! c (toks[i]!.toInt!)
        i := i + 1
      grid := grid.set! r row
    let r1 := toks[i]!.toNat! - 1; i := i + 1
    let c1 := toks[i]!.toNat! - 1; i := i + 1
    let r2 := toks[i]!.toNat! - 1; i := i + 1
    let c2 := toks[i]!.toNat! - 1; i := i + 1
    let res := bfs grid P Q r1 c1 r2 c2
    let line := match res with
      | some m => s!"The shortest path is {m} steps long."
      | none   => "Mission impossible!"
    parseCases toks i (t-1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (fun s => s ≠ "") |> Array.ofList
  let T := toks[0]!.toNat!
  let outs := parseCases toks 1 T []
  for line in outs do
    IO.println line
