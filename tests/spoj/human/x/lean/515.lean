/- Solution for SPOJ CLTZ - Collatz
https://www.spoj.com/problems/CLTZ/
-/
import Std
open Std

partial def collatzLen (n : Nat) : Nat :=
  let rec go (x count : Nat) : Nat :=
    if x == 1 then
      count + 1
    else if x % 2 == 0 then
      go (x / 2) (count + 1)
    else
      go (3 * x + 1) (count + 1)
  go n 0

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  for s in toks do
    let n := s.toNat!
    let l := collatzLen n
    IO.println l
