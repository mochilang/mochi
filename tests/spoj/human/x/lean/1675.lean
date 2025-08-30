/- Solution for SPOJ FUSION - Fusion Cube
https://www.spoj.com/problems/FUSION/
-/
import Std
open Std

/-- Process tokens for all test cases, returning array of outputs. -/
partial def solve (arr : Array String) (idx t : Nat) (acc : Array String) : Array String :=
  if t = 0 then acc
  else
    let K := (arr.get! idx).toNat!
    let N := (arr.get! (idx+1)).toNat!
    let rec loop (i idx best : Nat) : Nat × Nat :=
      if i = K then (best, idx)
      else
        let x := (arr.get! idx).toNat!
        let y := (arr.get! (idx+1)).toNat!
        let z := (arr.get! (idx+2)).toNat!
        let m1 := max x (N - x)
        let m2 := max y (N - y)
        let m3 := max z (N - z)
        let mx := max m1 (max m2 m3)
        let best := Nat.min best mx
        loop (i+1) (idx+3) best
    let (best, idx') := loop 0 (idx+2) N
    solve arr idx' (t-1) (acc.push (toString best))

/-- Main entry point: read input, compute answers and print them. -/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let arr := Array.ofList toks
  let t := (arr.get! 0).toNat!
  let outs := solve arr 1 t #[]
  for line in outs do
    IO.println line
