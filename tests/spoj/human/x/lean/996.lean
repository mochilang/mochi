/- Solution for SPOJ CFRAC - Continuous Fractions
https://www.spoj.com/problems/CFRAC/
-/

import Std
open Std

def repeatChar (c : Char) (n : Nat) : String :=
  String.mk (List.replicate n c)

def contFrac (p q : Nat) : List Nat :=
  let rec go (p q : Nat) : List Nat :=
    let a := p / q
    let r := p % q
    if r == 0 then [a] else a :: go q r
  let rec lastElem : List Nat → Nat
    | [] => 0
    | [x] => x
    | _ :: xs => lastElem xs
  let rec dropLast : List Nat → List Nat
    | [] => []
    | [x] => []
    | x :: xs => x :: dropLast xs
  let cf := go p q
  let last := lastElem cf
  if last == 1 then cf else dropLast cf ++ [last - 1, 1]

def build : List Nat → List String
  | [1] => ["1"]
  | a :: tail =>
      let tailLines := build tail
      let wTail := tailLines.head!.length
      let aStr := toString a
      let start := aStr.length + 3
      let w := start + wTail
      let numPos := start + (wTail - 1) / 2
      let top := repeatChar '.' numPos ++ "1" ++ repeatChar '.' (w - numPos - 1)
      let second := s!"{aStr}.+." ++ repeatChar '-' wTail
      let below := tailLines.map (fun line => repeatChar '.' start ++ line)
      top :: second :: below
  | _ => []

def solve (p q : Nat) : List String :=
  build (contFrac p q)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let mut idx := 0
  let mut case := 1
  while idx + 1 < toks.size do
    let p := toks.get! idx |>.toNat!
    let q := toks.get! (idx+1) |>.toNat!
    idx := idx + 2
    if p == 0 && q == 0 then
      break
    IO.println s!"Case {case}:"
    IO.println s!"{p} / {q}"
    for line in solve p q do
      IO.println line
    case := case + 1
