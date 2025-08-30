/- Solution for SPOJ BACKUP - Backup Files
https://www.spoj.com/problems/BACKUP/
-/

import Std
open Std

private def INF : Nat := (1 <<< 60)

private def solveCase (n k : Nat) (xs : Array Nat) : Nat :=
  Id.run do
    let m := n - 1
    let mut dp2 := Array.replicate (k+1) INF
    let mut dp1 := Array.replicate (k+1) INF
    let mut dp0 := Array.replicate (k+1) INF
    dp2 := dp2.set! 0 0
    for i in [0:m] do
      let cost := xs[i+1]! - xs[i]!
      dp0 := Array.replicate (k+1) INF
      dp0 := dp0.set! 0 0
      for j in [1:k+1] do
        let skip := dp1[j]!
        let take := dp2[j-1]! + cost
        let v := if skip < take then skip else take
        dp0 := dp0.set! j v
      dp2 := dp1
      dp1 := dp0
    return dp1[k]!

private def readAll : IO (Array Nat) := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "")
  return toks.map (·.toNat!) |>.toArray

partial def process (vals : Array Nat) (idx cases : Nat) (acc : Array String) : Array String :=
  if cases == 0 then acc else
    let n := vals[idx]!
    let k := vals[idx+1]!
    let xs := (Array.range n).map (fun i => vals[idx+2+i]!)
    let idx' := idx + 2 + n
    let res := solveCase n k xs
    process vals idx' (cases - 1) (acc.push (toString res))

def main : IO Unit := do
  let vals ← readAll
  let t := vals[0]!
  let lines := process vals 1 t #[]
  for s in lines do
    IO.println s
