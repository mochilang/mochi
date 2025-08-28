/- Solution for SPOJ DECORATE - Decorate the wall
https://www.spoj.com/problems/DECORATE/
-/

import Std
open Std

structure Rect where
  x1 : Int
  y1 : Int
  x2 : Int
  y2 : Int

def nonOverlap (x y w' h' : Int) (r : Rect) : Bool :=
  x + w' ≤ r.x1 ∨ r.x2 ≤ x ∨ y + h' ≤ r.y1 ∨ r.y2 ≤ y

def place (w h w' h' : Int) (rects : Array Rect) : Option (Int × Int) :=
  Id.run do
    let mut xs : Array Int := #[0]
    let mut ys : Array Int := #[0]
    for r in rects do
      xs := xs.push r.x2
      let l := r.x1 - w'
      if l ≥ 0 then
        xs := xs.push l
      ys := ys.push r.y2
      let b := r.y1 - h'
      if b ≥ 0 then
        ys := ys.push b
    xs := xs.filter (fun x => x ≥ 0 ∧ x + w' ≤ w) |>.qsort (fun a b => a < b)
    ys := ys.filter (fun y => y ≥ 0 ∧ y + h' ≤ h) |>.qsort (fun a b => a < b)
    for y in ys do
      for x in xs do
        let ok := rects.all (fun r => nonOverlap x y w' h' r)
        if ok then
          return some (x, y)
    return none

partial def solve (tokens : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (tokens[idx]!).toNat!
    let w : Int := (tokens[idx+1]!).toNat!
    let h : Int := (tokens[idx+2]!).toNat!
    let mut i := idx + 3
    let mut rects : Array Rect := #[]
    for _ in [0:n] do
      let x1 : Int := (tokens[i]!).toNat!
      let y1 : Int := (tokens[i+1]!).toNat!
      let x2 : Int := (tokens[i+2]!).toNat!
      let y2 : Int := (tokens[i+3]!).toNat!
      rects := rects.push {x1:=x1, y1:=y1, x2:=x2, y2:=y2}
      i := i + 4
    let w' : Int := (tokens[i]!).toNat!
    let h' : Int := (tokens[i+1]!).toNat!
    i := i + 2
    match place w h w' h' rects with
    | some (x, y) => IO.println s!"{x} {y}"
    | none => IO.println "Fail!"
    solve tokens i (t - 1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let tokensList := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")
  let tokens : Array String := tokensList.toArray
  let t := (tokens[0]!).toNat!
  solve tokens 1 t
