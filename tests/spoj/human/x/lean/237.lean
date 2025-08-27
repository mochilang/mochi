/- Solution for SPOJ SUMITR - Sums in a Triangle
https://www.spoj.com/problems/SUMITR/
-/

import Std
open Std

/-- process a single test case: maximum path sum in triangle --/
partial def solveOne (toks : Array String) (idx : Nat) : (Nat × Nat) :=
  let n := toks[idx]!.toNat!
  let mut i := idx + 1
  -- first row
  let first := toks[i]!.toNat!
  i := i + 1
  let mut dp : Array Nat := #[first]
  -- remaining rows
  for r in [1:n] do
    let len := r + 1
    let mut ndp := Array.mkArray len 0
    for j in [0:len] do
      let x := toks[i]!.toNat!
      i := i + 1
      let left := if j == 0 then 0 else dp.get! (j - 1)
      let right := if j == r then 0 else dp.get! j
      ndp := ndp.set! j (Nat.max left right + x)
    dp := ndp
  let ans := dp.foldl Nat.max 0
  (ans, i)

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse
  else
    let (res, idx) := solveOne toks idx
    solveAll toks idx (t-1) (toString res :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
