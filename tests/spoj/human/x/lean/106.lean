/- Solution for SPOJ BINSTIRL - Binary Stirling Numbers
https://www.spoj.com/problems/BINSTIRL/
-/

import Std
open Std

-- Return S(n,m) mod 2 using bit criterion
private def stirlingParity (n m : Nat) : Nat :=
  let mask := (m - 1) >>> 1
  if ((n - m) &&& mask) = 0 then 1 else 0

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
  match toks with
  | [] => pure ()
  | dStr :: rest =>
      let d := dStr.toNat!
      let arr := rest.toArray
      let mut idx := 0
      for _ in [0:d] do
        let n := arr[idx]!.toNat!
        let m := arr[idx+1]!.toNat!
        IO.println (stirlingParity n m)
        idx := idx + 2
