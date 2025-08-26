/- Solution for SPOJ IMP - The Imp
https://www.spoj.com/problems/IMP/
-/

import Std
open Std

structure Vec where
  x : Int
  y : Int

def norm2 (v : Vec) : Int := v.x * v.x + v.y * v.y

def dot (a b : Vec) : Int := a.x * b.x + a.y * b.y

def l1 (v : Vec) : Int :=
  Int.ofNat v.x.natAbs + Int.ofNat v.y.natAbs

def roundDiv (a b : Int) : Int :=
  if b = 0 then 0 else
  let q := a / b
  let r := a - q * b
  if 2 * r ≥ b then q + 1 else q

partial def gauss : Vec → Vec → Vec × Vec
  | v1, v2 =>
    if v1.x = 0 ∧ v1.y = 0 then (v2, v1)
    else if v2.x = 0 ∧ v2.y = 0 then (v1, v2)
    else if norm2 v2 < norm2 v1 then gauss v2 v1
    else
      let μ := roundDiv (dot v1 v2) (norm2 v1)
      if μ = 0 then (v1, v2)
      else gauss v1 { x := v2.x - μ * v1.x, y := v2.y - μ * v1.y }

def solve (a b c d : Int) : Int :=
  let v1 := Vec.mk a b
  let v2 := Vec.mk c d
  let (b1, b2) := gauss v1 v2
  let candidates := [b1, b2, { x := b1.x + b2.x, y := b1.y + b2.y },
                                 { x := b1.x - b2.x, y := b1.y - b2.y }]
  let opt :=
    candidates.foldl
      (fun acc v =>
        if v.x = 0 ∧ v.y = 0 then acc else
        match acc with
        | none => some (l1 v)
        | some m => some (min m (l1 v)))
      none
  match opt with
  | some ans => ans
  | none => 0

partial def loop (h : IO.FS.Stream) : IO Unit := do
  try
    let line := (← h.getLine).trim
    if line.isEmpty then return
    let parts := line.split (· = ' ')
    let a := (parts.get! 0).toInt!
    let b := (parts.get! 1).toInt!
    let c := (parts.get! 2).toInt!
    let d := (parts.get! 3).toInt!
    IO.println (solve a b c d)
    loop h
  catch _ => pure ()

def main : IO Unit := do
  loop (← IO.getStdin)
