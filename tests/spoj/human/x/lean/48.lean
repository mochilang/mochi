/- Solution for SPOJ BEADS - Glass Beads
https://www.spoj.com/problems/BEADS/
-/

import Std
open Std

-- Booth's algorithm for lexicographically minimal rotation
private def minRotation (s : String) : Nat :=
  let arr : Array Char := Array.ofList s.data
  let d := arr ++ arr
  let n := arr.size
  let rec loop (i j k : Nat) : Nat :=
    if i >= n || j >= n then Nat.min i j else
    if k = n then Nat.min i j else
    let a := d.get! (i+k)
    let b := d.get! (j+k)
    if a = b then loop i j (k+1)
    else if a < b then loop i (j+k+1) 0
    else loop (i+k+1) j 0
  loop 0 1 0

partial def process (toks : Array String) (idx t : Nat) (acc : List String) :
    List String :=
  if t = 0 then acc.reverse else
    let s := toks.get! idx
    let pos := (minRotation s) % s.length + 1
    process toks (idx+1) (t-1) (toString pos :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := process toks 1 t []
  for o in outs do
    IO.println o
