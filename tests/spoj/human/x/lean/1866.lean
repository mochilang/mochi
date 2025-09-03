/- Solution for SPOJ MKPALS - Making Pals
https://www.spoj.com/problems/MKPALS/
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

/-- Given a string of digits, return the cost and resulting palindrome length. --/
def solve (s : String) : Nat × Nat :=
  let arr := s.toList.toArray
  let rev := arr.toList.reverse.toArray
  let lps := lcs arr rev
  let n := arr.size
  let cost := n - lps
  let len := n + cost
  (cost, len)

partial def loop (h : IO.FS.Stream) (idx : Nat) : IO Unit := do
  let line ← h.getLine
  if line.trim.isEmpty then
    pure ()
  else
    let seq := line.trim
    let (c, l) := solve seq
    IO.println s!"Case {idx}, sequence = {seq}, cost = {c}, length = {l}"
    loop h (idx + 1)

def main : IO Unit := do
  let h ← IO.getStdin
  loop h 1
