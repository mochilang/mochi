/- Solution for SPOJ FAVDICE - Favorite Dice
https://www.spoj.com/problems/FAVDICE/
-/

import Std
open Std

/-- compute harmonic number H_n --/
def harmonic (n : Nat) : Float :=
  let rec loop (i : Nat) (acc : Float) : Float :=
    if i = 0 then acc
    else loop (i - 1) (acc + 1.0 / Float.ofNat i)
  loop n 0.0

/-- expected number of throws for N-sided die to show all faces --/
def expected (n : Nat) : Float :=
  Float.ofNat n * harmonic n

/-- round to two decimals --/
def round2 (x : Float) : Float :=
  let y := x * 100.0
  let y := if y ≥ 0.0 then y + 0.5 else y - 0.5
  (Float.ofInt (Int.ofFloat y)) / 100.0

/-- format float with two decimal places --/
def fmt (x : Float) : String :=
  let r := round2 x
  let s := toString r
  if s.contains '.' then
    let parts := s.split (· = '.') |>.toArray
    let intPart := parts.get! 0
    let frac := parts.get! 1
    if frac.length = 1 then intPart ++ "." ++ frac ++ "0"
    else if frac.length = 2 then s
    else intPart ++ "." ++ frac.take 2
  else s ++ ".00"

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line := (← h.getLine).trim
    if line = "" then
      process h t
    else
      let n := line.toNat!
      IO.println (fmt (expected n))
      process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
