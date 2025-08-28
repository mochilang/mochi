/- Solution for SPOJ FASHION - Fashion Shows
https://www.spoj.com/problems/FASHION/
-/

import Std
open Std

def parseNums (s : String) : Array Nat :=
  (s.trim.split (· = ' ') |>.filter (· ≠ "") |>.map String.toNat!).toArray

def solveCase (menLine womenLine : String) : Nat :=
  let men := (parseNums menLine).qsort (· < ·)
  let women := (parseNums womenLine).qsort (· < ·)
  List.zip men.toList women.toList
    |>.map (fun p => p.fst * p.snd)
    |>.foldl (· + ·) 0

partial def loop (t : Nat) (h : IO.FS.Stream) : IO Unit := do
  if t = 0 then return ()
  let _nLine ← h.getLine -- number of participants (unused)
  let menLine ← h.getLine
  let womenLine ← h.getLine
  IO.println (solveCase menLine womenLine)
  loop (t - 1) h

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  loop (tLine.trim.toNat!) h
