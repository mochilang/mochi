/- Solution for SPOJ VFMUL - Very Fast Multiplication
https://www.spoj.com/problems/VFMUL/
-/

import Std
open Std

private def parseNat (s : String) : Nat :=
  s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

partial def solve (toks : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let a := parseNat (toks[idx]!)
    let b := parseNat (toks[idx+1]!)
    IO.println (a * b)
    solve toks (idx+2) (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := parseNat (toks[0]!)
    solve toks 1 t
