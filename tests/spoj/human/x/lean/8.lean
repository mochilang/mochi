/- Solution for SPOJ CMPLS - Complete the Sequence!
https://www.spoj.com/problems/CMPLS/
-/

import Std
open Std

def parseInts (s : String) : List Int :=
  s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r').filterMap (fun t =>
    if t.isEmpty then none else t.toInt?)

def allEqual : List Int -> Bool
| [] => true
| x :: xs => xs.all (fun y => y = x)

def diffs : List Int -> List Int
| [] => []
| [_] => []
| x :: y :: xs => (y - x) :: diffs (y :: xs)

partial def buildTable (row : List Int) (acc : List (List Int)) : List (List Int) :=
  let acc := row :: acc
  if row.length <= 1 || allEqual row then acc
  else buildTable (diffs row) acc

def extendOnce : List (List Int) -> List (List Int)
| [] => []
| bottom :: rest =>
    let bottom' := bottom ++ [bottom.getLast!]
    let (acc, _) := rest.foldl
      (fun (p : List (List Int) × List Int) row =>
        let (rows, below) := p
        let newVal := row.getLast! + below.getLast!
        let row' := row ++ [newVal]
        (row' :: rows, row'))
      ([], bottom')
    bottom' :: acc.reverse

partial def extendN (table : List (List Int)) : Nat -> List (List Int)
| 0 => table
| Nat.succ k => extendN (extendOnce table) k

def complete (seq : List Int) (c : Nat) : List Int :=
  let table := buildTable seq []
  let table := extendN table c
  let top := (table.reverse).head!
  top.drop seq.length

partial def runCases : Nat -> List Int -> List String -> List String
| 0, _, acc => acc.reverse
| Nat.succ k, tokens, acc =>
    match tokens with
    | s :: rest1 =>
      match rest1 with
      | c :: rest2 =>
        let sNat := Int.toNat s
        let cNat := Int.toNat c
        let seq := rest2.take sNat
        let rest3 := rest2.drop sNat
        let ext := complete seq cNat
        let line := String.intercalate " " (ext.map (fun x => toString x))
        runCases k rest3 (line :: acc)
      | _ => acc.reverse
    | _ => acc.reverse

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let ints := parseInts input
  match ints with
  | [] => pure ()
  | t :: rest =>
      let lines := runCases (Int.toNat t) rest []
      IO.println (String.intercalate "\n" lines)
