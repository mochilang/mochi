/- Solution for SPOJ NQUEEN - Yet Another N-Queen Problem
https://www.spoj.com/problems/NQUEEN/
-/

import Std
open Std

partial def place (n : Nat) (preset : Array Nat)
    (row cols d1 d2 : Nat) (ans : Array Nat) : Option (Array Nat) :=
  if row = n then
    some ans
  else
    let pre := preset[row]!
    if pre ≠ 0 then
      let c := pre - 1
      let bcol := Nat.shiftLeft 1 c
      let bd1  := Nat.shiftLeft 1 (row + c)
      let bd2  := Nat.shiftLeft 1 (row + (n - 1 - c))
      if (cols &&& bcol) ≠ 0 || (d1 &&& bd1) ≠ 0 || (d2 &&& bd2) ≠ 0 then
        none
      else
        place n preset (row+1) (cols ||| bcol) (d1 ||| bd1) (d2 ||| bd2)
          (ans.set! row (c+1))
    else
      let rec tryCols (cs : List Nat) : Option (Array Nat) :=
        match cs with
        | [] => none
        | c :: rest =>
            let bcol := Nat.shiftLeft 1 c
            let bd1  := Nat.shiftLeft 1 (row + c)
            let bd2  := Nat.shiftLeft 1 (row + (n - 1 - c))
            if (cols &&& bcol) = 0 && (d1 &&& bd1) = 0 && (d2 &&& bd2) = 0 then
              match place n preset (row+1) (cols ||| bcol) (d1 ||| bd1) (d2 ||| bd2)
                    (ans.set! row (c+1)) with
              | some sol => some sol
              | none => tryCols rest
            else
              tryCols rest
      tryCols (List.range n)

def solveCase (n : Nat) (preset : Array Nat) : Array Nat :=
  match place n preset 0 0 0 0 (Array.replicate n 0) with
  | some res => res
  | none => Array.replicate n 0

partial def readAll : IO (Array String) := do
  let stdin ← IO.getStdin
  let s ← stdin.readToEnd
  return s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
            |>.filter (· ≠ "")
            |> List.toArray

def main : IO Unit := do
  let toks ← readAll
  let mut idx := 0
  let mut out : Array String := #[]
  while idx < toks.size do
    let n := toks[idx]!.toNat!
    idx := idx + 1
    let mut pre : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      pre := pre.push (toks[idx]!.toNat!)
      idx := idx + 1
    let sol := solveCase n pre
    let line := String.intercalate " " (sol.toList.map (fun x => toString x))
    out := out.push line
  for line in out do
    IO.println line
