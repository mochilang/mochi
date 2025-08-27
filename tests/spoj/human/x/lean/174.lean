/- Solution for SPOJ PAINTTMP - Paint templates
https://www.spoj.com/problems/PAINTTMP/
-/

import Std
open Std

private def overlap (n x y : Nat) : Nat :=
  if (x >>> n) ≠ 0 || (y >>> n) ≠ 0 then
    0
  else
    Id.run do
      let mut dp : Array (Array Nat) := #[#[1,0], #[0,0]]
      for k in [0:n] do
        let xb := (x >>> k) &&& 1
        let yb := (y >>> k) &&& 1
        let mut next : Array (Array Nat) := #[#[0,0], #[0,0]]
        for cx in [0:2] do
          for cy in [0:2] do
            let count := dp[cx]![cy]!
            if count ≠ 0 then
              for x1 in [0:2] do
                for y1 in [0:2] do
                  if y1 <= x1 then
                    let sumx := x1 + xb + cx
                    let x2 := sumx &&& 1
                    let cx2 := sumx >>> 1
                    let sumy := y1 + yb + cy
                    let y2 := sumy &&& 1
                    let cy2 := sumy >>> 1
                    if y2 <= x2 then
                      let row := next[cx2]!
                      let prev := row[cy2]!
                      next := next.set! cx2 (row.set! cy2 (prev + count))
        dp := next
      return dp[0]![0]!

partial def parseAll (arr : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := arr[idx]!.toNat!
    let x := arr[idx+1]!.toNat!
    let y := arr[idx+2]!.toNat!
    IO.println (overlap n x y)
    parseAll arr (idx+3) (t-1)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := toks[0]!.toNat!
    parseAll toks 1 t
