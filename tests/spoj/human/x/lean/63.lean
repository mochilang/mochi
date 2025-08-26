/- Solution for SPOJ SQRBR - Square Brackets
https://www.spoj.com/problems/SQRBR/
-/

import Std
open Std

/-- Count valid bracket sequences of length `2*n` with forced openings. -/
def count (n : Nat) (forced : Array Bool) : Nat :=
  let m := 2 * n
  Id.run do
    let mut dp := Array.replicate (n+2) 0
    dp := dp.set! 0 1
    for i in [0:m] do
      let mut ndp := Array.replicate (n+2) 0
      for b in [0:n+1] do
        let v := dp[b]!
        if v > 0 then
          if forced[i]! then
            ndp := ndp.set! (b+1) (ndp[b+1]! + v)
          else
            ndp := ndp.set! (b+1) (ndp[b+1]! + v)
            if b > 0 then
              ndp := ndp.set! (b-1) (ndp[b-1]! + v)
      dp := ndp
    return dp[0]!

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let n := toks[idx]!.toNat!
    let k := toks[idx+1]!.toNat!
    let size := 2 * n
    let (force, j) := Id.run do
      let mut arr := Array.replicate size false
      let mut j := idx + 2
      for _ in [0:k] do
        let p := toks[j]!.toNat!
        arr := arr.set! (p-1) true
        j := j + 1
      return (arr, j)
    let res := count n force
    solveAll toks j (t-1) (toString res :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
