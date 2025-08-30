/- Solution for SPOJ PRMLX - Permalex
https://www.spoj.com/problems/PRMLX/
-/
import Std
open Std

-- factorial of a natural number
@[simp] def fact : Nat → Nat
| 0 => 1
| (n+1) => (n+1) * fact n

-- count permutations of multiset represented by counts of letters
partial def permCount (counts : Array Nat) : Nat :=
  let total := counts.foldl (fun t c => t + c) 0
  counts.foldl (fun acc c => acc / fact c) (fact total)

-- compute lexicographic rank (1-based) of a string with possible duplicates
partial def rankString (s : String) : Nat :=
  let chars := s.toList
  let mut counts : Array Nat := Array.mkArray 26 0
  for c in chars do
    let idx := Char.toNat c - Char.toNat 'a'
    counts := counts.modify idx (fun x => x + 1)
  let rec loop (lst : List Char) (counts : Array Nat) (acc : Nat) : Nat :=
    match lst with
    | [] => acc + 1
    | c :: rest =>
        let idx := Char.toNat c - Char.toNat 'a'
        let rec smaller (i : Nat) (acc : Nat) : Nat :=
          if h : i < idx then
            let acc := if counts[i]! > 0 then
              let counts' := counts.modify i (fun x => x - 1)
              acc + permCount counts'
            else acc
            smaller (i+1) acc
          else acc
        let acc := smaller 0 acc
        let counts := counts.modify idx (fun x => x - 1)
        loop rest counts acc
  loop chars counts 0

-- produce a string padded on the left to width 10
partial def pad10 (n : Nat) : String :=
  let s := toString n
  let rec spaces : Nat → String
  | 0 => ""
  | (k+1) => " " ++ spaces k
  spaces (10 - s.length) ++ s

-- main I/O loop
partial def mainLoop : IO Unit := do
  let line ← IO.getStdin.getLine
  if line == "#" then
    pure ()
  else if line.trim == "" then
    IO.println "" *> mainLoop
  else
    let r := rankString line.trim
    IO.println (pad10 r) *> mainLoop

def main : IO Unit :=
  mainLoop
