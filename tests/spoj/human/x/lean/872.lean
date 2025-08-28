/- Solution for SPOJ MARKUP - Mark-up
https://www.spoj.com/problems/MARKUP/
-/

import Std
open Std

partial def skipDigits (s : String) (i : Nat) : Nat :=
  if h : i < s.length then
    let c := s.get ⟨i, h⟩
    if c.isDigit then
      skipDigits s (i+1)
    else
      i
  else
    i

partial def skipNum (s : String) (i : Nat) : Nat :=
  let j := skipDigits s i
  if h : j < s.length then
    let c := s.get ⟨j, h⟩
    if c == '.' then
      skipDigits s (j+1)
    else
      j
  else
    j

partial def processAux (s : String) (i : Nat) (proc : Bool) (acc : String) : String :=
  if h : i < s.length then
    let c := s.get ⟨i, h⟩
    if c == '\\' then
      if h2 : i + 1 < s.length then
        let d := s.get ⟨i+1, h2⟩
        if proc then
          if d == 'b' ∨ d == 'i' then
            processAux s (i+2) proc acc
          else if d == 's' then
            let j := skipNum s (i+2)
            processAux s j proc acc
          else if d == '*' then
            processAux s (i+2) (!proc) acc
          else
            processAux s (i+2) proc (acc.push d)
        else
          if d == '*' then
            processAux s (i+2) (!proc) acc
          else
            processAux s (i+2) proc (acc.push '\\'.push d)
      else
        if proc then
          processAux s (i+1) proc acc
        else
          processAux s (i+1) proc (acc.push '\\')
    else
      processAux s (i+1) proc (acc.push c)
  else
    acc

def process (s : String) : String :=
  processAux s 0 true ""

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  IO.print (process input)
