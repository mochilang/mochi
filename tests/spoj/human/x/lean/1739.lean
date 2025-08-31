/- Solution for SPOJ EQU2 - Yet Another Equation
https://www.spoj.com/problems/EQU2/
-/

import Std
open Std

partial def pell (n : Nat) : Nat × Nat :=
  let a0 := Nat.sqrt n
  let rec loop (m d a num1 num den1 den : Nat) : Nat × Nat :=
    if num * num = n * den * den + 1 then
      (num, den)
    else
      let m' := d * a - m
      let d' := (n - m' * m') / d
      let a' := (a0 + m') / d'
      let num2 := num1
      let den2 := den1
      let num1' := num
      let den1' := den
      let num' := a' * num1' + num2
      let den' := a' * den1' + den2
      loop m' d' a' num1' num' den1' den'
  loop 0 1 a0 1 a0 0 1

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    let (x, y) := pell n
    IO.println s!"{x} {y}"
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
