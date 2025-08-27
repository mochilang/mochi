/- Solution for SPOJ LONER - The Loner
https://www.spoj.com/problems/LONER/
-/

import Std
open Std

partial def canWin (s : String) (seen : Std.HashSet String := {}) : (Bool × Std.HashSet String) :=
  if seen.contains s then (false, seen) else
  let seen := seen.insert s
  let ones := s.foldl (fun acc c => if c == '1' then acc + 1 else acc) 0
  if ones == 1 then (true, seen) else
  let n := s.length
  let rec loop (i : Nat) (seen : Std.HashSet String) : (Bool × Std.HashSet String) :=
    if h : i + 3 ≤ n then
      let sub := s.extract i (i+3)
      if sub == "110" then
        let t := s.extract 0 i ++ "001" ++ s.extract (i+3) n
        let (r, seen) := canWin t seen
        if r then (true, seen) else loop (i+1) seen
      else if sub == "011" then
        let t := s.extract 0 i ++ "100" ++ s.extract (i+3) n
        let (r, seen) := canWin t seen
        if r then (true, seen) else loop (i+1) seen
      else
        loop (i+1) seen
    else
      (false, seen)
  loop 0 seen

def solveCase (s : String) : Bool := (canWin s).1

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then pure () else
  let nLine ← h.getLine
  if nLine.trim = "" then process h t else
  let _n := nLine.trim.toNat!
  let board := (← h.getLine).trim
  IO.println (if solveCase board then "yes" else "no")
  process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
