/- Solution for SPOJ AIBOHP - AIBOHPHOBIA
https://www.spoj.com/problems/AIBOHP/
-/

import Std
open Std

/-- Longest common subsequence length between two arrays of characters. --/
private def lcs (a b : Array Char) : Nat :=
  Id.run do
    let m := b.size
    let mut prev := Array.replicate (m+1) 0
    let mut curr := Array.replicate (m+1) 0
    for x in a do
      for j in [0:m] do
        let v :=
          if x = b[j]! then
            prev[j]! + 1
          else
            Nat.max (prev[j+1]!) (curr[j]!)
        curr := curr.set! (j+1) v
      prev := curr
      curr := Array.replicate (m+1) 0
    return prev[m]!

/-- Minimum insertions to make a string a palindrome. --/
private def minInsertions (s : String) : Nat :=
  let arr := s.toList.toArray
  let rev := arr.toList.reverse.toArray
  arr.size - lcs arr rev

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let s ← h.getLine
    IO.println (minInsertions s.trim)
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
