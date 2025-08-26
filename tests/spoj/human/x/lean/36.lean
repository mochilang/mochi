/- Solution for SPOJ CODE1 - Secret Code
https://www.spoj.com/problems/CODE1/
-/

import Std
open Std

-- compute ceiling of square root for a natural number
private def ceilSqrt (n : Nat) : Nat :=
  let r := Nat.sqrt n
  if r * r == n then r else r + 1

-- recursive conversion of a Gaussian integer to base br+ibi
partial def convert (xr xi br bi limit norm : Int) (depth : Nat := 0) : Option (List Int) := do
  if xr == 0 && xi == 0 then
    some []
  else if depth >= 100 then
    none
  else
    let rec tryDigit (r : Int) : Option (List Int) :=
      if r >= limit then none else
      let xr' := xr - r
      let nr := xr' * br + xi * bi
      let ni := xi * br - xr' * bi
      if nr % norm == 0 && ni % norm == 0 then
        let qr := nr / norm
        let qi := ni / norm
        match convert qr qi br bi limit norm (depth + 1) with
        | some ds => some (r :: ds)
        | none    => tryDigit (r + 1)
      else
        tryDigit (r + 1)
    tryDigit 0

-- solve one test case
private def solveCase (xr xi br bi : Int) : String :=
  if xr == 0 && xi == 0 then
    "0"
  else
    let norm := br*br + bi*bi
    let limit := Int.ofNat (ceilSqrt (Int.toNat norm))
    match convert xr xi br bi limit norm 0 with
    | some ds =>
        let ds := ds.reverse.map (fun d => d.toString)
        String.intercalate "," ds
    | none => "The code cannot be decrypted."

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let nums := line.trim.split (fun c => c = ' ')
                      |>.filter (fun s => s ≠ "")
                      |>.map (fun s => s.toInt!)
    match nums with
    | [xr, xi, br, bi] => IO.println (solveCase xr xi br bi)
    | _                => IO.println "The code cannot be decrypted."
    loop h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
