/- Solution for SPOJ DISUBSTR - Distinct Substrings
https://www.spoj.com/problems/DISUBSTR/
-/

import Std
open Std

/-- longest common prefix length of two strings -/
def lcp (a b : String) : Nat :=
  let rec go (l1 l2 : List Char) (i : Nat) : Nat :=
    match l1, l2 with
    | c1 :: t1, c2 :: t2 =>
        if c1 = c2 then go t1 t2 (i + 1) else i
    | _, _ => i
  go a.data b.data 0

/-- sum of LCPs of consecutive strings in a list -/
partial def sumLcp : List String -> Nat
  | [] => 0
  | [_] => 0
  | a :: b :: t => lcp a b + sumLcp (b :: t)

/-- number of distinct substrings of `s` -/
def countDistinctSubstrings (s : String) : Nat :=
  let suf := (List.range s.length).map (fun i => s.drop i)
  let sorted := suf.mergeSort (fun a b => decide (a <= b))
  let total := s.length * (s.length + 1) / 2
  total - sumLcp sorted

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let s := line.trim
    IO.println (countDistinctSubstrings s)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
