-- Solution for SPOJ ABWORDS - AB-words
-- https://www.spoj.com/problems/ABWORDS
import Std
open Std

-- simple quicksort for strings
partial def sortStrings : List String → List String
| [] => []
| x :: xs =>
    let rec partition (ys : List String) (l r : List String) :=
      match ys with
      | [] => (l, r)
      | y :: ys' =>
          if y < x then partition ys' (y :: l) r
          else partition ys' l (y :: r)
    let (l, r) := partition xs [] []
    sortStrings l ++ [x] ++ sortStrings r

private def canonical (s : String) : String :=
  let rec loop (cs : List Char) (stack : List (List String)) : List (List String) :=
    match cs with
    | [] => stack
    | c :: cs' =>
        if c = 'a' then
          loop cs' ([] :: stack)
        else
          match stack with
          | children :: parent :: rest =>
              let sorted := sortStrings children
              let combined := sorted.foldl (fun acc x => acc ++ x) ""
              let node := "(" ++ combined ++ ")"
              let parent := parent ++ [node]
              loop cs' (parent :: rest)
          | _ => stack
  let final := loop s.data [[]]
  match final with
  | [children] =>
      let sorted := sortStrings children
      sorted.foldl (fun acc x => acc ++ x) ""
  | _ => ""

partial def readNatSkipEmpty : IO Nat := do
  let rec loop : IO Nat := do
    let line ← (← IO.getStdin).getLine
    let s := line.trim
    if s.isEmpty then loop
    else
      match s.toNat? with
      | some v => return v
      | none => loop
  loop

partial def readWords (n : Nat) : IO (List String) := do
  let rec loop (k : Nat) (acc : List String) : IO (List String) := do
    if k = 0 then
      return acc
    else
      let line ← (← IO.getStdin).getLine
      let w := line.trim
      let canon := canonical w
      let acc := if acc.any (· = canon) then acc else canon :: acc
      loop (k - 1) acc
  loop n []

private def processCases (t : Nat) : IO Unit := do
  let rec loop (k : Nat) : IO Unit := do
    if k = 0 then return ()
    else
      let n ← readNatSkipEmpty
      let uniq ← readWords n
      IO.println uniq.length
      loop (k - 1)
  loop t

def main : IO Unit := do
  let t ← readNatSkipEmpty
  processCases t

