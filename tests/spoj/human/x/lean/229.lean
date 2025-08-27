/- Solution for SPOJ SORTING - Sorting is easy
https://www.spoj.com/problems/SORTING/
-/

import Std
open Std

/-- Sort the characters of a string in non-decreasing order. --/
def sortString (s : String) : String :=
  let arr := s.data.toArray.qsort (· ≤ ·)
  String.mk arr.toList

def main : IO Unit := do
  let h ← IO.getStdin
  let line ← h.getLine
  IO.println (sortString line.trim)
