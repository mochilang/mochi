/- Solution for SPOJ BALL - Billiard
https://www.spoj.com/problems/BALL/
-/

import Std
open Std

partial def solveCase (x y a b : Nat) : Nat :=
  let g := Nat.gcd (x-1) (y-1)
  if (a-1) % g == 0 && (b-1) % g == 0 then
    2
  else
    let s := (x-1) / g + (y-1) / g
    let base := 4 * s
    if s % 2 == 0 then base else base + 2

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    let [xStr, yStr, aStr, bStr] := line.trim.split (· == ' ') | []
    let x := xStr.toNat!
    let y := yStr.toNat!
    let a := aStr.toNat!
    let b := bStr.toNat!
    IO.println (toString <| solveCase x y a b)
    loop h (n-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
