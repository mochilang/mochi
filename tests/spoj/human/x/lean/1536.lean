/- Solution for SPOJ BLUEEQ3 - Help Blue Mary Please! (Act III)
https://www.spoj.com/problems/BLUEEQ3/
-/
import Std
open Std

/-- Return index 0-based for character ('A' -> 0 etc.). -/
private def idx (c : Char) : Nat := c.toNat - 'A'.toNat

/-- Try assigning digit `d` to letter index `i`. If already assigned to a
    different digit, return `none`. Otherwise return updated map and used. -/
private def tryAssign (map : Array (Option Nat)) (used : Array Bool)
    (i d : Nat) : Option (Array (Option Nat) × Array Bool) :=
  match map[i]! with
  | some v => if v = d then some (map, used) else none
  | none   => if used[d]! then none else
                some (map.set! i (some d), used.set! d true)

/-- All candidate digits for letter index `i` given current mapping. -/
private def candidates (map : Array (Option Nat)) (used : Array Bool)
    (i n : Nat) : List Nat :=
  match map[i]! with
  | some v => [v]
  | none   => (List.range n).filter (fun d => !(used[d]!))

/-- Find first `f x` that returns `some`. -/
private def firstSome {α β} : List α → (α → Option β) → Option β
  | [], _ => none
  | x :: xs, f => match f x with
                  | some y => some y
                  | none   => firstSome xs f

/-- Depth-first search to solve one case. -/
partial def dfs (a b c : Array Char) (n pos carry : Nat)
    (map : Array (Option Nat)) (used : Array Bool) : Option (Array Nat) :=
  if pos = n then
    if carry = 0 then some (map.map (fun o => o.getD 0)) else none
  else
    let j := n - 1 - pos
    let xi := idx a[j]!
    let yi := idx b[j]!
    let zi := idx c[j]!
    firstSome (candidates map used xi n) (fun dx =>
      match tryAssign map used xi dx with
      | none => none
      | some (map1, used1) =>
        firstSome (candidates map1 used1 yi n) (fun dy =>
          match tryAssign map1 used1 yi dy with
          | none => none
          | some (map2, used2) =>
            let s := dx + dy + carry
            let dz := s % n
            let carry' := s / n
            match tryAssign map2 used2 zi dz with
            | none => none
            | some (map3, used3) =>
              dfs a b c n (pos + 1) carry' map3 used3))

/-- Solve a single test case. -/
private def solveCase (n : Nat) (sa sb sc : String) : Array Nat :=
  let a := sa.toList.toArray
  let b := sb.toList.toArray
  let c := sc.toList.toArray
  match dfs a b c n 0 0 (Array.replicate n none) (Array.replicate n false) with
  | some arr => arr
  | none => #[]

/-- Parse all test cases and output solutions. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun ch => ch = ' ' ∨ ch = '\n' ∨ ch = '\t' ∨ ch = '\r')
                 |>.filter (fun s => s ≠ "")
                 |> List.toArray
  let rec loop (i : Nat) (outs : List String) : List String :=
    if h : i + 3 < toks.size then
      let n := (toks[i]!).toNat!
      let sa := toks[i+1]!
      let sb := toks[i+2]!
      let sc := toks[i+3]!
      let mapping := solveCase n sa sb sc
      let line := String.intercalate " " (mapping.toList.map (fun d => toString d))
      loop (i + 4) (line :: outs)
    else
      outs.reverse
  let lines := loop 0 []
  for line in lines do
    IO.println line

