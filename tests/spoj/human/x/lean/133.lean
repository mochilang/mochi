/- Solution for SPOJ ABWORDS - AB-words
https://www.spoj.com/problems/ABWORDS/
-/

import Std
open Std

inductive Tree where
  | node (children : List Tree)

partial def parseAux : List Char → (List Char × Tree)
| [] => ([], .node [])
| 'a' :: cs =>
  let rec parseChildren : List Char → List Tree → (List Char × List Tree)
  | [], acc => ([], acc.reverse)
  | 'b' :: rest, acc => (rest, acc.reverse)
  | cs, acc =>
      let (rest1, child) := parseAux cs
      parseChildren rest1 (child :: acc)
  let (rest, kids) := parseChildren cs []
  (rest, .node kids)
| _ :: cs => (cs, .node [])  -- unreachable for nice words

partial def canon : Tree → String
| .node children =>
  let canons := children.map canon
  let sorted := canons.qsort (· < ·)
  "a" ++ String.join sorted ++ "b"

def canonicalWord (s : String) : String :=
  (canon (parseAux s.data).snd)

partial def solveCases (toks : Array String) (idx t : Nat)
    (acc : Array String) : Array String :=
  if t = 0 then acc else
    let n := toks.get! idx |>.toNat!
    let mut set : Std.HashSet String := {}
    let mut i := idx + 1
    for _ in [0:n] do
      let w := toks.get! i
      set := set.insert (canonicalWord w)
      i := i + 1
    let acc := acc.push (toString set.size)
    solveCases toks i (t - 1) acc

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filter (· ≠ "") |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solveCases toks 1 t #[]
  for line in outs do
    IO.println line
