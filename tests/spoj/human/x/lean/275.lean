/- Solution for SPOJ WATERWAY - The Water Ringroad
https://www.spoj.com/problems/WATERWAY/
-/

import Std
open Std

partial def antiDist (s : Array Char) (n start dest : Nat) : Option Nat :=
  let rec loop (i steps : Nat) :=
    if i == dest then some steps
    else
      let idx := i % n
      let c := s.get! idx
      if c == 'A' || c == 'B' then
        let i' := (i + 1) % n
        loop i' (steps + 1)
      else
        none
  loop start 0

partial def clockDist (s : Array Char) (n start dest : Nat) : Option Nat :=
  let rec loop (i steps : Nat) :=
    if i == dest then some steps
    else
      let idx := (i + n - 1) % n
      let c := s.get! idx
      if c == 'C' || c == 'B' then
        loop idx (steps + 1)
      else
        none
  loop start 0

def addAnti (cnt : Array Nat) (start steps n : Nat) : Array Nat :=
  let rec loop (cnt : Array Nat) (i steps : Nat) :=
    match steps with
    | 0 => cnt
    | _ =>
      let idx := i % n
      let cnt := cnt.modify idx (· + 1)
      loop cnt (i + 1) (steps - 1)
  loop cnt start steps

def addClock (cnt : Array Nat) (start steps n : Nat) : Array Nat :=
  let rec loop (cnt : Array Nat) (i steps : Nat) :=
    match steps with
    | 0 => cnt
    | _ =>
      let idx := (i + n - 1) % n
      let cnt := cnt.modify idx (· + 1)
      loop cnt idx (steps - 1)
  loop cnt start steps

partial def solveCase (n : Nat) (s : Array Char) : Nat :=
  let cnt := Array.mkArray n 0
  let rec outer (i : Nat) (cnt : Array Nat) :=
    if h : i < n then
      let rec inner (j : Nat) (cnt : Array Nat) :=
        if h2 : j < n then
          if i == j then
            inner (j + 1) cnt
          else
            let cnt :=
              match antiDist s n i j, clockDist s n i j with
              | some da, some dc =>
                  if da <= dc then addAnti cnt i da n else addClock cnt i dc n
              | some da, none => addAnti cnt i da n
              | none, some dc => addClock cnt i dc n
              | none, none => cnt
            inner (j + 1) cnt
        else cnt
      let cnt := inner 0 cnt
      outer (i + 1) cnt
    else cnt
  let cnt := outer 0 cnt
  cnt.foldl (fun m x => if x > m then x else m) 0

partial def parseCases (lines : List String) : List (Nat × Array Char) :=
  let rec loop (ls : List String) (acc : List (Nat × Array Char)) :=
    match ls with
    | [] => acc.reverse
    | nStr :: sStr :: rest =>
        let n := nStr.toNat!
        loop rest ((n, sStr.data.toArray) :: acc)
    | _ => acc.reverse
  match lines with
  | tStr :: rest =>
      let t := tStr.toNat!
      loop (rest.take (2 * t)) []
  | [] => []

def main : IO Unit := do
  let input ← IO.readStdin
  let lines := input.trim.split (fun c => c = '\n' || c = '\r') |>.filter (fun s => s ≠ "")
  let cases := parseCases lines
  for (n, s) in cases do
    IO.println (solveCase n s)
