/- Solution for SPOJ CFRAC2 - Continuous Fractions Again
https://www.spoj.com/problems/CFRAC2/
-/

import Std
open Std

/-- Extract continued fraction coefficients from the ASCII matrix representation. -/
def extractCoeffs (lines : Array String) : List Nat :=
  Id.run do
    let mut coeffs : List Nat := []
    let mut lastDigits : String := ""
    for line in lines do
      let chars := line.data
      match chars.findIdx? (· = '+') with
      | some idx =>
          let left := chars.take idx
          let digits := (left.reverse.dropWhile (fun c => !c.isDigit)).takeWhile Char.isDigit |>.reverse
          let num := String.mk digits
          coeffs := coeffs ++ [num.toNat!]
      | none =>
          let digits := chars.filter Char.isDigit
          if digits ≠ [] then
            lastDigits := String.mk digits
    if lastDigits ≠ "" then
      coeffs := coeffs ++ [lastDigits.toNat!]
    return coeffs

/-- Evaluate a continued fraction given its coefficients. -/
def evalCF (cs : List Nat) : Nat × Nat :=
  match cs.reverse with
  | [] => (0,1)
  | a :: rest =>
      rest.foldl (fun (p,q) b => (b * p + q, p)) (a, 1)

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line := (← h.getLine).trim
  let parts := line.split (· = ' ') |>.filter (· ≠ "")
  let m := parts[0]! |>.toNat!
  let n := parts[1]! |>.toNat!
  if m = 0 ∧ n = 0 then
    pure ()
  else
    let mut arr : Array String := #[]
    for _ in [0:m] do
      let l := (← h.getLine).trimRight
      arr := arr.push l
    let coeffs := extractCoeffs arr
    let (p,q) := evalCF coeffs
    IO.println s!"{p} {q}"
    loop h

/-- Entry point -/
def main : IO Unit := do
  let h ← IO.getStdin
  loop h
