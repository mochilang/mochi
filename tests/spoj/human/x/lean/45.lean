/- Solution for SPOJ COMMEDIA - Commedia dell Arte
https://www.spoj.com/problems/COMMEDIA/
-/
import Std
open Std

def parity (perm : Array Nat) : Bool :=
  let n := perm.size
  let mut visited : Array Bool := Array.mkArray n false
  let mut swaps := 0
  for i in [0:n] do
    if !visited.get! i then
      let mut j := i
      let mut cycle := 0
      while !visited.get! j do
        visited := visited.set! j true
        j := perm.get! j
        cycle := cycle + 1
      swaps := swaps + cycle - 1
  swaps % 2 == 1

partial def solve (toks : Array String) (idx : Nat) (cases : Nat)
    (acc : List String) : List String :=
  if cases == 0 then acc.reverse else
    let m := toks.get! idx |>.toNat!
    let n := m * m * m
    let mut perm : Array Nat := Array.mkArray n 0
    let mut blank := 0
    let mut j := idx + 1
    for i in [0:n] do
      let v := toks.get! j |>.toNat!
      if v == 0 then
        perm := perm.set! i (n - 1)
        blank := i
      else
        perm := perm.set! i (v - 1)
      j := j + 1
    let odd := parity perm
    let x := blank % m
    let y := (blank / m) % m
    let z := blank / (m * m)
    let lhs := (if odd then 1 else 0) + x + y + z
    let rhs := m - 1
    let line := if lhs % 2 == rhs % 2 then
      "Puzzle can be solved."
    else
      "Puzzle is unsolvable."
    solve toks j (cases - 1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solve toks 1 t []
  for line in outs do
    IO.println line
