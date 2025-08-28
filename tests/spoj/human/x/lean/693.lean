/- Solution for SPOJ LWAR - Lethal Warfare
https://www.spoj.com/problems/LWAR/
-/

import Std
open Std

/-- Parse a binary string into a natural number. --/
def parseBin (s : String) : Nat :=
  s.data.foldl (fun acc c => acc * 2 + (if c = '1' then 1 else 0)) 0

/-- Convert a natural number to a binary string of fixed width. --/
def toBin (n width : Nat) : String :=
  let rec loop (k m : Nat) (acc : List Char) :=
    if k = 0 then acc
    else
      let bit : Char := if m % 2 = 1 then '1' else '0'
      loop (k-1) (m / 2) (bit :: acc)
  String.mk (loop width n [])

/-- Compute the bombing order for ship `x` among `2^n` ships. --/
partial def bombOrder (n x : Nat) : Nat :=
  if n = 0 then 0
  else
    let rec loop (n x acc half : Nat) : Nat :=
      if n = 0 then acc
      else if n = 1 then acc + x
      else if x < half then acc + x
      else
        let x := x - half
        let acc := acc + half
        let quarter := half / 2
        if x >= quarter then
          acc + (2 * quarter - 1 - x)
        else
          loop (n - 2) x (acc + quarter) (quarter / 2)
    loop n x 0 (Nat.shiftLeft 1 (n - 1))

/-- Entry point --/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let s := (← h.getLine).trim
    let n := s.length
    let x := parseBin s
    let ord := bombOrder n x
    IO.println (toBin ord n)
