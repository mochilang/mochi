/- Solution for SPOJ CATM - The Cats and the Mouse
https://www.spoj.com/problems/CATM/
-/

import Std
open Std

/-- Manhattan distance between two grid points --/
def dist (x1 y1 x2 y2 : Nat) : Nat :=
  Int.natAbs (Int.ofNat x1 - Int.ofNat x2) +
  Int.natAbs (Int.ofNat y1 - Int.ofNat y2)

/-- check if the mouse can escape the cats --/
def canEscape (n m mx my c1x c1y c2x c2y : Nat) : Bool :=
  Id.run do
    let mut escape := false
    -- left and right edges
    for r in [1:n+1] do
      if !escape then
        let dm := dist mx my r 1
        let dc1 := dist c1x c1y r 1
        let dc2 := dist c2x c2y r 1
        if dm < Nat.min dc1 dc2 then
          escape := true
        else
          let dm2 := dist mx my r m
          let dc1' := dist c1x c1y r m
          let dc2' := dist c2x c2y r m
          if dm2 < Nat.min dc1' dc2' then
            escape := true
    -- top and bottom edges
    for c in [1:m+1] do
      if !escape then
        let dm := dist mx my 1 c
        let dc1 := dist c1x c1y 1 c
        let dc2 := dist c2x c2y 1 c
        if dm < Nat.min dc1 dc2 then
          escape := true
        else
          let dm2 := dist mx my n c
          let dc1' := dist c1x c1y n c
          let dc2' := dist c2x c2y n c
          if dm2 < Nat.min dc1' dc2' then
            escape := true
    return escape

partial def solve (toks : Array Nat) (idx t n m : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let mx := toks[idx]!
    let my := toks[idx+1]!
    let c1x := toks[idx+2]!
    let c1y := toks[idx+3]!
    let c2x := toks[idx+4]!
    let c2y := toks[idx+5]!
    let ans := if canEscape n m mx my c1x c1y c2x c2y then "YES" else "NO"
    solve toks (idx+6) (t-1) n m (ans :: acc)

/-- Entry point --/
def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.map (fun s => s.toNat!)
                |>.toArray
  let n := toks[0]!
  let m := toks[1]!
  let k := toks[2]!
  let out := solve toks 3 k n m []
  IO.println (String.intercalate "\n" out)
